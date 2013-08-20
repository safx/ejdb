/**************************************************************************************************
 *  C/C++ API for EJDB database library http://ejdb.org
 *  Copyright (C) 2012-2013 Softmotions Ltd <info@softmotions.com>
 *
 *  This file is part of EJDB.
 *  EJDB is free software; you can redistribute it and/or modify it under the terms of
 *  the GNU Lesser General Public License as published by the Free Software Foundation; either
 *  version 2.1 of the License or any later version.  EJDB is distributed in the hope
 *  that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 *  License for more details.
 *  You should have received a copy of the GNU Lesser General Public License along with EJDB;
 *  if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 *  Boston, MA 02111-1307 USA.
 *************************************************************************************************/

#include "tcbpool.h"
#include "myconf.h"
#include "tcutil.h"
#include <assert.h>
#include <fcntl.h>

#define BPFILEMODE    00644             // permission of created files

#define BPHDRMAGIC 0xdc2a               //BP extent magic data
#define BPHDRSIZ  256                   //BPEXT: magic(2) + maxsize(64) + size(64) + ppow(1) + nextext(1) + extra(124)
#define BPHDREXTRAOFF 132               //Offset of extra extent header data

#define BPBPOWDEF 6                     //Default BP buffer aligment power 64B
#define BPBPOWMAX 14                    //Maximum of BP buffer aligment power 16K
#define BPDEFMAXSIZE   0x80000000       //Default extent max size 2147483648


typedef struct { /** BP extent */
    char *fpath; /**<Path to first BP extent */
    HANDLE fd; /**< Extent file handle */
    uint32_t hdrsiz; /**< Size of custom app header */
    uint64_t maxsize; /*< Max size of extent */
    uint64_t size; /*< Current size of extent */
    uint8_t ppow; /**< The power of buffer aligment */
    uint8_t nextext; /*< If set to 0x01 this extent continued by next extent */
    struct BPEXT *next; /**< Next BP extent */
    void *mmtx; /**< Global BP mutex */
} BPEXT;

struct BPOOL { /** BP itself */
    BPOPTS opts; /**< BP options */
    bpstate_t state; /**< BP state */
    BPEXT *ext; /**< First BP extent */
};

/*************************************************************************************************
 *                           Static function prototypes
 *************************************************************************************************/

static int _openext(BPEXT *ext, const char *fpath, tcomode_t omode, TCBPINIT init, void *initop);
static int _closext(BPEXT *ext);
static int _destroyext(BPEXT *ext);
static int _creatext(BPEXT** ext);
static int _loadmeta(BPEXT *ext, const char *buf);
static int _dumpmeta(BPEXT *ext, char *buf);

BPOOL* tcbpnew() {
    BPOOL *ret;
    TCMALLOC(ret, sizeof (*ret));
    memset(ret, 0, sizeof (*ret));
    return ret;
}

bool tcbpisopen(BPOOL *bp) {
    return (bp && bp->ext && !INVALIDHANDLE(bp->ext->fd));
}

void tcbpdel(BPOOL *bp) {
    assert(bp);
    if (tcbpisopen(bp)) {
        tcbpclose(bp);
    }
    TCFREE(bp);
}

int tcbpopen(BPOOL *bp, const char *fpath, tcomode_t omode, TCBPINIT init, void *initop) {
    assert(bp && init);
    int rv = TCESUCCESS;
    if (omode == 0) {
        omode = TCOREADER | TCOWRITER | TCOCREAT;
    }
    //Initialize main extent
    rv = _creatext(&(bp->ext));
    if (rv) {
        goto finish;
    }
    rv = _openext(bp->ext, fpath, omode, init, initop);
    if (rv) {
        _destroyext(bp->ext);
        goto finish;
    }

finish:
    return rv;
}

int tcbpclose(BPOOL *bp) {
    return TCESUCCESS;
}

/*************************************************************************************************
 *                                Private staff
 *************************************************************************************************/

static int _loadmeta(BPEXT *ext, const char *buf) {
    //BPEXT: magic(3) + maxsize(64) + size(64) + nextext(1) + extra(124)
    int rp = 0;
    uint16_t magic = 0;
    memcpy(&magic, buf + rp, sizeof(magic));
    magic = TCITOHL(magic);
    if (magic != BPHDRMAGIC) {
        return TCEMETA;
    }
    rp += sizeof(magic);
    memcpy(&(ext->maxsize), buf + rp, sizeof(ext->maxsize));
    ext->maxsize = TCITOHLL(ext->maxsize);
    rp += sizeof(ext->maxsize);

    memcpy(&(ext->size), buf + rp, sizeof(ext->size));
    ext->size = TCITOHLL(ext->size);
    rp += sizeof(ext->size);

    memcpy(&(ext->ppow), buf + rp, sizeof(ext->ppow));
    rp += sizeof(ext->ppow);
    if (ext->ppow == 0 || ext->ppow > BPBPOWMAX) {
        return TCEMETA;
    }

    memcpy(&(ext->nextext), buf, sizeof(ext->nextext));
    rp += sizeof(ext->nextext);
    if (ext->nextext & ~0x01) {
        return TCEMETA;
    }
    return TCESUCCESS;
}

static int _dumpmeta(BPEXT *ext, char *buf) {
    memset(buf, 0, BPHDRSIZ - BPHDREXTRAOFF);
    int wp = 0;
    uint16_t snum;
    snum = BPHDRMAGIC;
    snum = TCHTOIS(snum);
    memcpy(buf, &snum, sizeof(snum));
    wp += sizeof(snum);

    uint64_t llnum;
    llnum = ext->maxsize;
    llnum = TCHTOILL(llnum);
    memcpy(buf + wp, &llnum, sizeof(llnum));
    wp += sizeof(llnum);

    llnum = ext->size;
    llnum = TCHTOILL(llnum);
    memcpy(buf + wp, &llnum, sizeof(llnum));
    wp += sizeof(llnum);

    memcpy(buf + wp, &(ext->ppow), sizeof(ext->ppow));
    wp += sizeof(ext->ppow);

    memcpy(buf + wp, &(ext->nextext), sizeof(ext->nextext));
    return TCESUCCESS;
}

static int _creatext(BPEXT** ext) {
    int rv = TCESUCCESS;
    BPEXT *e;
    TCMALLOC(e, sizeof (*e));
    memset(e, 0, sizeof (*e));
    TCMALLOC(e->mmtx, sizeof (pthread_rwlock_t));
    if (pthread_rwlock_init(e->mmtx, NULL) != 0) {
        rv = TCETHREAD;
    }
    if (rv) {
        TCFREE(e);
        e = NULL;
    }
    *ext = e;
    return rv;
}

static int _closext(BPEXT *ext) {
    assert(ext);
    if (!INVALIDHANDLE(ext->fd)) {
        CLOSEFH(ext->fd);
    }
    return TCESUCCESS;
}

static int _destroyext(BPEXT *ext) {
    assert(ext);
    if (ext->mmtx) {
        pthread_mutex_destroy(ext->mmtx);
        TCFREE(ext->mmtx);
    }
    TCFREE(ext);
    return TCESUCCESS;
}

static int _openext(BPEXT *ext, const char *fpath, tcomode_t omode, TCBPINIT init, void *initop) {
    int rv = TCESUCCESS;
    char hbuf[BPHDRSIZ]; //BPE header buffer
    BPOPTS opts = {0};
    struct stat sbuf; //BPE file stat buff

#ifndef _WIN32
    int mode = O_RDONLY;
    if (omode & TCOWRITER) {
        mode = O_RDWR;
        if (omode & TCOCREAT) mode |= O_CREAT;
    }
    assert(!ext->fd);
    ext->fd = open(fpath, mode, BPFILEMODE);
#else
    DWORD mode, cmode;
    mode = GENERIC_READ;
    cmode = OPEN_EXISTING;
    if (omode & TCBPOWRITE) {
        mode |= GENERIC_WRITE;
        if (omode & TCBOTRUNC) {
            cmode = CREATE_ALWAYS;
        } else if (omode & TCBPOCREATE) {
            cmode = OPEN_ALWAYS;
        }
    }
    fd = CreateFile(path, mode,
            FILE_SHARE_READ | FILE_SHARE_WRITE,
            NULL, cmode, FILE_ATTRIBUTE_NORMAL, NULL);
#endif
    if (INVALIDHANDLE(ext->fd)) {
        rv = TCEOPEN;
        goto finish;
    }
    if (init) {
        //typedef bool (*TCBPINIT) (HANDLE fd, tcomode_t omode, uint32_t *hdrsiz, BPOPTS *opts, void *opaque);
        if (!init(ext->fd, omode, &(ext->hdrsiz), &opts, initop)) {
            rv = TCBPEXTINIT;
            goto finish;
        }
    }
    if (ext->hdrsiz > 0) {
        if (!tcfseek(ext->fd, ext->hdrsiz, TCFSTART)) {
            rv = TCESEEK;
            goto finish;
        }
    }
    if (fstat(ext->fd, &sbuf)) {
        rv = TCESTAT;
        goto finish;
    }
    if (sbuf.st_size > ext->hdrsiz) { //trying to read the existing header
        if (!tcread(ext->fd, hbuf, BPHDRSIZ)) {
            rv = TCEREAD;
            goto finish;
        }
        rv = _loadmeta(ext, hbuf);
        if (rv) {
            goto finish;
        }

    } else { //init new meta
        ext->maxsize = opts.maxsize > 0 ? opts.maxsize : BPDEFMAXSIZE;
        ext->ppow = (opts.bpow > 0 && opts.bpow <= BPBPOWMAX) ? opts.bpow : BPBPOWDEF;
        assert(!ext->size);
        assert(!ext->nextext);
        rv = _dumpmeta(ext, hbuf);
        if (rv) {
            goto finish;
        }
        if (!tcwrite(ext->fd, hbuf, BPHDRSIZ)) {
            rv = TCEWRITE;
            goto finish;
        }
    }

finish:
    if (rv) { //error code
        if (!INVALIDHANDLE(ext->fd)) {
            CLOSEFH(ext->fd);
        }
    }
    return rv;
}
