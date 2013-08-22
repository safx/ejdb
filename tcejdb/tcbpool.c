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
#define BPHDRSIZ  256                   //BPEXT: magic(2) + maxsize(64) + size(64) + bpow(1) + nextext(1) + extra(124)
#define BPHDREXTRAOFF 132               //Offset of extra extent header data

#define BPPPOWMAX 16                    //Maximum BP page size power 64K
#define BPPPOWDEF 12                    //Default BP page size power 4K
#define BPPPOWMIN 10                    //Minimum BP page size power 1K
#define BPBPOWDEF 6                     //Default BP buffer aligment power 64B
#define BPBPOWMAX 14                    //Maximum of BP buffer aligment power 16K
#define BPDEFMAXSIZE   0x80000000       //Default extent max size 2G
#define BPEXTMINSIZE   0x4000000        //Default extent min size 64M 

struct CPAGE { /** Cached page */
    off_t id; /**< Page off in number of pages */
    bool pinned; /**< If page is pinned */
    char *data; /**< Pointer to page data */
};

struct UPAGE { /** Page in Use */
    off_t id; /**< Page ID */
    int refs; /**< Page refs count */
    pthread_cond_t cv; /**< Condition var for thread sync */
};
typedef struct UPAGE UPAGE;

struct BPEXT { /** BP extent */
    char *fpath; /**<Path to first BP extent */
    HANDLE fd; /**< Extent file handle */
    uint64_t goff; /**< Global extent offset */
    uint64_t maxsize; /*< Max size of extent */
    uint64_t size; /*< Current size of extent */
    uint8_t bpow; /**< The power of buffer aligment */
    uint8_t ppow; /**< Page size pow */
    uint8_t nextext; /*< If set to 0x01 this extent continued by next extent */
    BPEXT *next; /**< Next BP extent */
    BPOOL *bp; /**< BP ref */
    volatile int fatalcode; /**< Last happen fatal ERROR code */
    uint32_t apphdrsz; /**< Size of custom app header */
    char *apphdrdata; /**< Custom app header data */

    pthread_mutex_t *upagesmtx; /**< Page-lock mutex */
    pthread_mutex_t *freeblocksmtx; /**< Free-space mutex */

    TCTREE *upages; /**< Pages in use */
};

struct BPOOL { /** BP itself */
    bpstate_t state; /**< BP state */
    BPEXT *ext; /**< First BP extent */
    pthread_rwlock_t *mlock;  /**< Method RW-lock */
};

/*************************************************************************************************
 *                           Static function prototypes
 *************************************************************************************************/

static int _openext(BPEXT *ext, const char *fpath, tcomode_t omode, TCBPINIT init, void *initop);
static int _closext(BPEXT *ext);
static int _destroyext(BPEXT *ext);
static int _creatext(BPOOL *bp, BPEXT** ext);
static int _loadmeta(BPEXT *ext, const char *buf);
static int _dumpmeta(BPEXT *ext, char *buf);
//Select extent for write|read operation
static int _selectextent(BPOOL *bp, BPEXT **ext, uint64_t off, size_t len);

int tcbpnew(BPOOL** _bp) {
    int rv = TCESUCCESS;
    BPOOL *bp;
    TCMALLOC(bp, sizeof (*bp));
    memset(bp, 0, sizeof (*bp));

    TCMALLOC(bp->mlock, sizeof(pthread_rwlock_t));
    rv = pthread_rwlock_init(bp->mlock, NULL);
    if (rv) {
        goto finish;
    }

finish:
    *_bp = rv ? NULL : bp;
    if (rv && bp) {
        tcbpdel(bp);
    }
    return rv;
}

bool tcbpisopen(BPOOL *bp) {
    return (bp && (bp->ext) && !INVALIDHANDLE(bp->ext->fd));
}

void tcbpdel(BPOOL *bp) {
    assert(bp);
    if (tcbpisopen(bp)) {
        tcbpclose(bp);
    }
    TCFREE(bp);
}

int tcbpapphdrsiz(BPOOL *bp) {
    if (!tcbpisopen(bp)) {
        return 0;
    }
    if (bp->ext) {
        return bp->ext->apphdrsz;
    }
    return 0;
}

int tcbpreadcutomhdrdata(BPOOL *bp, char *buf, int off, int len) {
    if (!tcbpisopen(bp)) {
        return 0;
    }
    BPEXT *ext = bp->ext;
    if (ext && ext->apphdrsz > 0) {
        int sz = MIN(ext->apphdrsz - off, len);
        if (sz > 0) {
            memcpy(buf, ext->apphdrdata + off, sz);
            return sz;
        }
    }
    return 0;
}

int tcbpwritecustomhdrdata(BPOOL *bp, int hoff, char *buf, int boff, int len) {
    if (!tcbpisopen(bp)) {
        return 0;
    }
    BPEXT *ext = bp->ext;
    if (ext && ext->apphdrsz > 0) {
        int sz = MIN(ext->apphdrsz - hoff, len);
        if (sz > 0) {
            memcpy(ext->apphdrdata + hoff, buf + boff, len);
            return sz;
        }
    }
    return 0;
}

int tcbpopen(BPOOL *bp, const char *fpath, tcomode_t omode, TCBPINIT init, void *initop) {
    //todo lock?
    assert(bp && init);
    if (tcbpisopen(bp)) {
        return TCBPEOPENED;
    }
    int rv = TCESUCCESS;
    if (omode == 0) {
        omode = TCOREADER | TCOWRITER | TCOCREAT;
    }
    //Initialize main extent
    rv = _creatext(bp, &(bp->ext));
    if (rv) {
        goto finish;
    }
    rv = _openext(bp->ext, fpath, omode, init, initop);
    if (rv) {
        _destroyext(bp->ext);
        bp->ext = NULL;
        goto finish;
    }
    BPEXT *ext = bp->ext;
    int c = 1;
    while (!rv && ext->nextext) {
        BPEXT *next;
        rv = _creatext(bp, &next);
        if (rv) {
            ext->fatalcode = rv;
            goto finish;
        }
        char *efile = tcsprintf("%s.%d", ext->fpath, c);
        rv = _openext(next, efile, omode, NULL, NULL);
        TCFREE(efile);
        ext->next = next;
        ext = next;
        ++c;
    }
finish:
    if (rv) { //opened with errors and should be closed
        tcbpclose(bp);
    }
    return rv;
}

int tcbpsync(BPOOL *bp) {
    int rv = TCESUCCESS;
    //todo
    return rv;
}

int tcbpclose(BPOOL *bp) {
    assert(bp);
    if (!tcbpisopen(bp)) {
        return TCBPECLOSED;
    }
    tcbpsync(bp);
    //_lockallextents(bp, true);
    int rv = TCESUCCESS;
    BPEXT *ext = bp->ext;
    while (ext) {
        int crv = _closext(ext);
        if (crv) {
            rv = crv;
        }
        ext = ext->next;
    }
    //_unlockallextents(bp);
    return rv;
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
        ext->fatalcode = TCEMETA;
        return TCEMETA;
    }
    rp += sizeof(magic);
    memcpy(&(ext->maxsize), buf + rp, sizeof(ext->maxsize));
    ext->maxsize = TCITOHLL(ext->maxsize);
    rp += sizeof(ext->maxsize);

    memcpy(&(ext->size), buf + rp, sizeof(ext->size));
    ext->size = TCITOHLL(ext->size);
    rp += sizeof(ext->size);

    memcpy(&(ext->bpow), buf + rp, sizeof(ext->bpow));
    rp += sizeof(ext->bpow);
    if (ext->bpow == 0 || ext->bpow > BPBPOWMAX) {
        ext->fatalcode = TCEMETA;
        return TCEMETA;
    }

    memcpy(&(ext->nextext), buf + rp, sizeof(ext->nextext));
    rp += sizeof(ext->nextext);
    if (ext->nextext & ~0x01) {
        ext->fatalcode = TCEMETA;
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

    memcpy(buf + wp, &(ext->bpow), sizeof(ext->bpow));
    wp += sizeof(ext->bpow);

    memcpy(buf + wp, &(ext->nextext), sizeof(ext->nextext));
    return TCESUCCESS;
}

static int _creatext(BPOOL *bp, BPEXT** ext) {
    int rv = TCESUCCESS;
    BPEXT *e;
    TCMALLOC(e, sizeof (*e));
    memset(e, 0, sizeof (*e));

    TCMALLOC(e->upagesmtx, sizeof(pthread_mutex_t));
    rv = pthread_mutex_init(e->upagesmtx, NULL);
    if (rv) {
        goto finish;
    }
    TCMALLOC(e->freeblocksmtx, sizeof(pthread_mutex_t));
    rv = pthread_mutex_init(e->freeblocksmtx, NULL);

finish:
    if (rv) {
        _destroyext(e);
        e = NULL;
    }
    e->bp = bp;
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
    if (ext->apphdrdata) {
        TCFREE(ext->apphdrdata);
    }
    if (ext->upagesmtx) {
        pthread_mutex_destroy(ext->upagesmtx);
        TCFREE(ext->upagesmtx);
    }
    if (ext->freeblocksmtx) {
        pthread_mutex_destroy(ext->freeblocksmtx);
        TCFREE(ext->freeblocksmtx);
    }
    TCFREE(ext);
    return TCESUCCESS;
}

static int _openext(BPEXT *ext, const char *fpath, tcomode_t omode, TCBPINIT init, void *initop) {
    assert(ext && ext->bp);
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
    if (init) { //It is first extent
        //typedef bool (*TCBPINIT) (HANDLE fd, tcomode_t omode, uint32_t *apphdrsz, BPOPTS *opts, void *opaque);
        if (!init(ext->fd, omode, &(ext->apphdrsz), &opts, initop)) {
            rv = TCBPEXTINIT;
            goto finish;
        }
        ext->ppow = (opts.ppow >= BPPPOWMIN && opts.ppow <= BPPPOWMAX) ? opts.ppow : BPPPOWDEF;
        if (ext->apphdrsz > 0) {
            if (!tcfseek(ext->fd, ext->apphdrsz, TCFSTART)) {
                rv = TCESEEK;
                goto finish;
            }
        }
    }
    if (ext->apphdrsz > 0) {
        if (!tcfseek(ext->fd, 0, TCFSTART)) {
            rv = TCESEEK;
            goto finish;
        }
        TCCALLOC(ext->apphdrdata, ext->apphdrsz, 1);
        if (!tcread(ext->fd, ext->apphdrdata, ext->apphdrsz)) {
            rv = TCEREAD;
            goto finish;
        }
    }
    if (fstat(ext->fd, &sbuf)) {
        rv = TCESTAT;
        goto finish;
    }
    if (sbuf.st_size > ext->apphdrsz) { //trying to read the existing header
        if (!tcread(ext->fd, hbuf, BPHDRSIZ)) {
            rv = TCEREAD;
            goto finish;
        }
        rv = _loadmeta(ext, hbuf);
        if (rv) {
            goto finish;
        }

    } else { //init new meta
        ext->maxsize = (opts.maxsize > 0 ? (opts.maxsize < BPEXTMINSIZE ? BPEXTMINSIZE : opts.maxsize) : BPDEFMAXSIZE);
        ext->bpow = (opts.bpow > 0 && opts.bpow <= BPBPOWMAX) ? opts.bpow : BPBPOWDEF;
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
        ext->fatalcode = rv;
    }
    return rv;
}


static int _selectextent(BPOOL *bp, BPEXT **ext, uint64_t off, size_t len) {
    assert(bp && ext && bp->ext);
    int rv = TCESUCCESS;
    BPEXT *e = bp->ext;
    uint64_t eoff = 0;
    while(e) {
        if (len >= e->maxsize) {
            return TCBPEBLKOVERFLOW;
        }
        if (off >= eoff && off + len <= eoff + e->maxsize) {
            break;
        }
        eoff += e->maxsize;
        e = e->next;
    }
    *ext = e;
    return rv;
}

int tcbplock(BPOOL *bp, BPEXT **ext, uint64_t off, size_t len, bool wr) {
    int rv = _selectextent(bp, ext, off, len);
    if (rv) {
        goto finish;
    }
    BPEXT *e = *ext;
    if (e == NULL) {
        
    }
    
finish:
    return rv;
}

int tcbpunlock(BPEXT *ext, uint64_t addr, size_t len) {
    int rv = TCESUCCESS;

    return rv;
}

int tcbpread(BPOOL *bp, uint64_t off, size_t len, char *out) {
    assert(bp && bp->ext);
    //if (((int) off & ((1 << bp->ext->bpow) - 1)) == 0) {
    //    return TCBPEADDRALIGN;
    //}
    int rv = TCESUCCESS;

    return rv;
}
