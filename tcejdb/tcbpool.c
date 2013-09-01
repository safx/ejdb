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
#define BPHDRSIZ  256                   //BPEXT: magic(2) + maxsize(64) + size(64) + 
                                        //       bpow(1) + ppow(1) + incpow(1) + nextext(1) + extra(122)
#define BPHDREXTRAOFF 134               //Offset of extra extent header data

#define BPPPOWMAX 17                    //Maximum BP page size power 128K
#define BPPPOWDEF 12                    //Default BP page size power 4K
#define BPPPOWMIN 10                    //Minimum BP page size power 1K
#define BPBPOWDEF 7                     //Default BP buffer aligment power 128B
#define BPBPOWMAX 14                    //Maximum of BP buffer aligment power 16K
#define BPPOWINCDEF 19                  //Default file size increment pow 512K
#define BPPOWINCMAX 27                  //Max file size increment 128M
#define BPPOWINCMIN 1                   //Min file size increment 2B

#define BPDEFMAXSIZE   0x80000000       //Default extent max size 2G
#define BPEXTMINSIZE   0x4000000        //Default extent min size 64M 
#define BPIOBUFSIZ     16384            //IO buffer size

#define ROUNDUP(BP_x, BP_v) ((((~(BP_x)) + 1) & ((BP_v) - 1)) + (BP_x))
#define PSIZE(BP_ext) (1 << (BP_ext)->ppow)
#define BSIZE(BP_ext) (1 << (BP_ext)->bpow)
#define INCSIZE(BP_ext) (1 << (BP_ext)->incpow)
#define FBSIZE(BP_ext) (((BP_ext)->maxsize / BSIZE(BP_ext)) / 8)


#define BPLOCKMETHOD(BP_bp, BP_wr) (BP_bp->mmtx ? _bplockmeth(BP_bp, BP_wr) : 0)
#define BPUNLOCKMETHOD(BP_bp) (BP_bp->mmtx ? _bpunlockmeth(BP_bp) : 0)
#define BPLOCKEXT(BP_bp) (BP_bp->extmtx ? _bplockext(BP_bp) : 0)
#define BPUNLOCKEXT(BP_bp) (BP_bp->extmtx ? _bpunlockext(BP_bp) : 0)

struct LPAGE { /** Page in Use */
    off_t id; /**< Page ID */
    int refs; /**< Page refs count */
    int wrefs; /**< Conditional wait refs */
    pthread_cond_t cv; /**< Condition var for thread sync */
};

struct BPEXT { /** BP extent */
    char *fpath; /**<Path to first BP extent */
    HANDLE fd; /**< Extent file handle */
    int64_t goff; /**< Global extent offset */
    int64_t maxsize; /*< Max size of extent */
    int64_t size; /*< Current size of extent */
    uint8_t bpow; /**< The power of buffer aligment */
    uint8_t ppow; /**< Page size pow */
    uint8_t nextext; /*< If set to 0x01 this extent continued by next extent */
    uint8_t incpow; /*< File size increment pow */
    BPEXT *next; /**< Next BP extent */
    BPOOL *bp; /**< BP ref */
    volatile int fatalcode; /**< Last happen fatal ERROR code */

    uint32_t apphdrsz; /**< Size of custom app header */
    TCMMAP hdrmmap;    /**< MMAP for ext header data */
    //char *apphdrdata; /**< Custom app header data */

    pthread_mutex_t *fblocksmtx; /**< Free-space mutex */
    pthread_mutex_t *lpagesmtx; /**< Page-lock mutex */
    TCTREE *lpages; /**< Pages currently locked by threads */
};

struct BPOOL { /** BP itself */
    bpstate_t state; /**< BP state */
    BPEXT *ext; /**< First BP extent */
    pthread_rwlock_t *mmtx;  /**< Method RW-lock */
    pthread_mutex_t *extmtx; /**< Extents mutex */
    tcomode_t omode; /**< BP open mode */
};


/*************************************************************************************************
 *                           Static function prototypes
 *************************************************************************************************/
EJDB_INLINE int _bplockmeth(BPOOL *bp, bool wr);
EJDB_INLINE int _bpunlockmeth(BPOOL *bp);
EJDB_INLINE int _bplockext(BPOOL *bp);
EJDB_INLINE int _bpunlockext(BPOOL *bp);

EJDB_STATIC int _extopen(BPEXT *ext, const char *fpath, tcomode_t omode, TCBPINIT init, void *initop);
EJDB_STATIC int _extclose(BPEXT *ext);
EJDB_STATIC int _extdel(BPEXT *ext);
EJDB_STATIC int _extnew(BPOOL *bp, BPEXT** ext);
EJDB_STATIC int _extsetmtx(BPEXT *ext);
EJDB_STATIC int _loadmeta(BPEXT *ext, const char *buf);
EJDB_INLINE int _extsyncmeta(BPEXT *ext);
EJDB_STATIC int _extdumpmeta(BPEXT *ext, char *buf);
EJDB_STATIC int _extdumpmeta2(BPEXT *ext);
EJDB_STATIC int _extcreate(BPEXT *prev, BPEXT *next, int64_t end);
EJDB_STATIC int _extensurend(BPEXT *ext, int64_t end);
EJDB_STATIC int _extselect(BPOOL *bp, BPEXT **ext, int64_t off, size_t len, bool wr);
EJDB_STATIC int _bpsync(BPOOL *bp);
EJDB_STATIC int _extplock(BPEXT *ext, int64_t off, size_t len, bool wr);
EJDB_STATIC int _extpunlock(BPEXT *ext, int64_t off, size_t len);
EJDB_STATIC int _extpnumlock(BPEXT *ext, int pnum, bool wr);
EJDB_STATIC int _extpnumunlock(BPEXT *ext, int pnum);
EJDB_STATIC int _extpagescmp(const char *aptr, int asiz, const char *bptr, int bsiz, void *op);
EJDB_INLINE size_t _extdataoff(BPEXT *ext);
EJDB_STATIC bool _initintext(HANDLE fd, tcomode_t omode, uint32_t *apphdrsz, BPOPTS *opts, void *opaque);

int tcbpnew(BPOOL** _bp) {
    int rv = TCESUCCESS;
    BPOOL *bp;
    TCMALLOC(bp, sizeof (*bp));
    memset(bp, 0, sizeof (*bp));
    *_bp = rv ? NULL : bp;
    if (rv && bp) {
        tcbpdel(bp);
    }
    return rv;
}

int tcbpsetmtx(BPOOL *bp) {
    TCMALLOC(bp->mmtx, sizeof(pthread_rwlock_t));
    int rv = pthread_rwlock_init(bp->mmtx, NULL);
    if (!rv) {
        TCMALLOC(bp->extmtx, sizeof(pthread_mutex_t));
        pthread_mutex_init(bp->extmtx, NULL);
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
    if (bp->mmtx) {
        pthread_rwlock_destroy(bp->mmtx);
        TCFREE(bp->mmtx);
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

int tcbpapphdread(BPOOL *bp, void *buf, int off, int len) {
    if (BPLOCKMETHOD(bp, false)) {
        return 0;
    }
    if (!tcbpisopen(bp)) {
        _bpunlockmeth(bp);
        return 0;
    }
    BPEXT *ext = bp->ext;
    if (ext && ext->apphdrsz > 0) {
        int sz = MIN(ext->apphdrsz - off, len);
        if (sz > 0) {
            memcpy(buf, ext->hdrmmap.data + off, sz);
            BPUNLOCKMETHOD(bp);
            return sz;
        }
    }
    BPUNLOCKMETHOD(bp);
    return 0;
}

int tcbpapphdwrite(BPOOL *bp, int hoff, char *buf, int boff, int len) {
    if (BPLOCKMETHOD(bp, false)) {
        return 0;
    }
    if (!tcbpisopen(bp)) {
        BPUNLOCKMETHOD(bp);
        return 0;
    }
    BPEXT *ext = bp->ext;
    if (ext && ext->apphdrsz > 0) {
        int sz = MIN(ext->apphdrsz - hoff, len);
        if (sz > 0) {
            memcpy(ext->hdrmmap.data + hoff, buf + boff, len);
            BPUNLOCKMETHOD(bp);
            return sz;
        }
    }
    BPUNLOCKMETHOD(bp);
    return 0;
}

int tcbpopen(BPOOL *bp, const char *fpath, tcomode_t omode, TCBPINIT init, void *initop) {
    assert(bp);
    if (tcbpisopen(bp)) {
        return TCBPEOPENED;
    }
    int rv = TCESUCCESS;
    if (omode == 0) {
        omode = BPDEFOMODE;
    }
    rv = BPLOCKMETHOD(bp, true);
    if (rv) return rv;
    bp->omode = omode;
    //Initialize main extent
    rv = _extnew(bp, &(bp->ext));
    if (rv) {
        goto finish;
    }
    rv = _extopen(bp->ext, fpath, omode, init, initop);
    if (rv) {
        _extdel(bp->ext);
        bp->ext = NULL;
        goto finish;
    }
    BPEXT *ext = bp->ext;
    int c = 1;
    while (!rv && ext->nextext) {
        BPEXT *next;
        rv = _extnew(bp, &next);
        if (rv) {
            ext->fatalcode = rv;
            goto finish;
        }
        char *efile = tcsprintf("%s.%d", ext->fpath, c);
        rv = _extopen(next, efile, omode, NULL, NULL);
        TCFREE(efile);
        ext->next = next;
        ext = next;
        ++c;
    }
finish:
    BPUNLOCKMETHOD(bp);
    if (rv) { //opened with errors and should be closed
        tcbpclose(bp);
    }
    return rv;
}

int tcbpsync(BPOOL *bp) {
    int rv = TCESUCCESS;
    rv = BPLOCKMETHOD(bp, true);
    if (rv) return rv;
    rv = _bpsync(bp);
    BPUNLOCKMETHOD(bp);
    return rv;
}

int tcbpclose(BPOOL *bp) {
    assert(bp);
    BPLOCKMETHOD(bp, true);
    if (!tcbpisopen(bp)) {
        BPUNLOCKMETHOD(bp);
        return TCBPECLOSED;
    }
    tcbpsync(bp);
    int rv = TCESUCCESS;
    BPEXT *ext = bp->ext;
    while (ext) {
        int crv = _extclose(ext);
        if (crv) {
            rv = crv;
        }
        ext = ext->next;
    }
    BPUNLOCKMETHOD(bp);
    return rv;
}

int tcbplock(BPOOL *bp, BPEXT **ext, int64_t off, size_t len, bool wr) {
    if (((int) off & (BSIZE(bp->ext) - 1)) == 0) {
        return TCBPEADDRALIGN;
    }
    int rv = BPLOCKMETHOD(bp, false);
    if (rv) return rv;
    BPEXT *e;
    if (!ext) {
        ext = &e;
    }
    rv = _extselect(bp, ext, off, len, wr);
    if (rv) goto finish;
    e = *ext;
    assert(e);
    while (e) {
        int64_t extmaxlen = (e->maxsize + e->goff - off);
        int64_t locklen = MIN(extmaxlen, len);
        rv = _extplock(e, off, locklen, wr);
        if (rv) break;
        off += locklen;
        len -= locklen;
        e =  (len > 0) ? e->next : NULL;
    }
    if (len > 0) {
        rv = TCBPEXTNOTFOUND;
    }
finish:
    BPUNLOCKMETHOD(bp);
    return rv;
}

int tcbpunlock(BPOOL *bp, int64_t off, size_t len) {
    if (((int) off & (BSIZE(bp->ext) - 1)) != 0) {
        return TCBPEADDRALIGN;
    }
    int rv = BPLOCKMETHOD(bp, false);
    if (rv) return rv;
    BPEXT *ext;
    rv = _extselect(bp, &ext, off, len, false);
    if (rv) goto finish;
    BPEXT *e = ext;
    assert(e);
    while (e) {
        int64_t extmaxlen = (e->maxsize + e->goff - off);
        int64_t locklen = MIN(extmaxlen, len);
        rv = _extpunlock(e, off, locklen);
        if (rv) break;
        off += locklen;
        len -= locklen;
        e =  (len > 0) ? e->next : NULL;
    }
finish:
    BPUNLOCKMETHOD(bp);
    return rv;
}

int tcbplockread(BPOOL *bp, int64_t off, size_t len, char *out, size_t *sp) {
    int rv = tcbplock(bp, NULL, off, len, false);
    if (rv) return rv;
    rv = tcbpread(bp, off, len, out, sp);
    int rv2 = tcbpunlock(bp, off, len);
    return rv ? rv : rv2;
}

int tcbpread(BPOOL *bp, int64_t off, size_t len, char *out, size_t *sp) {
    assert(bp && bp->ext);
    *sp = 0;
    int rv = BPLOCKMETHOD(bp, false);
    if (rv) return rv;

    char *sout = out; //save original len
    BPEXT *ext;
    rv = _extselect(bp, &ext, off, len, false);
    if (rv) goto finish;
    BPEXT *e = ext;
    assert(e);

    while (e) {
        int64_t extmaxlen = (e->maxsize + e->goff - off);
        int64_t locklen = MIN(extmaxlen, len);
        int rlen = locklen;
        int roff = (off - e->goff);
        while (true) {
            int rb = pread(e->fd, out, rlen, roff);
            if (rb >= rlen) {
                out += rlen;
                break;
            } else if (rb > 0) {
                out += rb;
                rlen -= rb;
                roff += rb;
            } else if (rb == -1) {
                if (errno != EINTR) {
                    rv = TCEREAD;
                    break;
                }
            } else {
                if (rlen > 0) {
                    rv = TCEREAD;
                    break;
                }
            }
        }
        if (rv) break;
        off += locklen;
        len -= locklen;
        e =  (len > 0) ? e->next : NULL;
    }
    *sp = (out - sout);

finish:
    BPUNLOCKMETHOD(bp);
    return rv;
}

int tcbplockwrite(BPOOL *bp, int64_t off, const void *buf, size_t len, size_t *sp) {
    int rv = tcbplock(bp, NULL, off, len, true);
    if (rv) return rv;
    rv = tcbpwrite(bp, off, buf, len, sp);
    int rv2 = tcbpunlock(bp, off, len);
    return rv ? rv : rv2;
}

int tcbpwrite(BPOOL *bp, int64_t off, const void *buf, size_t len, size_t *sp) {
    assert(bp && bp->ext);
    *sp = 0;
    if (!(bp->omode & TCOWRITER)) {
        return TCBPERONLY;
    }
    int rv = BPLOCKMETHOD(bp, false);
    if (rv) return rv;

    size_t slen = len; //save original len
    BPEXT *ext;
    rv = _extselect(bp, &ext, off, len, true);
    if (rv) goto finish;
    BPEXT *e = ext;
    assert(e);

    while (e) {
        int64_t extmaxlen = (e->maxsize + e->goff - off);
        int64_t locklen = MIN(extmaxlen, len);
        int wlen = locklen;
        int woff = (off - e->goff);
        while (true) {
            int wb = pwrite(e->fd, buf, wlen, woff);
            if (wb >= wlen) {
                buf = (char *) buf + wlen;
                break;
            } else if (wb > 0) {
                buf = (char *) buf + wb;
                wlen -= wb;
                woff += wb;
            } else if (wb == -1) {
                if (errno != EINTR) {
                    rv = TCEWRITE;
                    break;
                }
            } else {
                if (wlen > 0) {
                    rv = TCEWRITE;
                    break;
                }
            }
        }
        if (rv) break;
        off += locklen;
        len -= locklen;
        e =  (len > 0) ? e->next : NULL;
    }
    *sp = (slen - len);

finish:
    BPUNLOCKMETHOD(bp);
    return rv;
}

/*************************************************************************************************
 *                                Private staff
 *************************************************************************************************/

/**
 * Init internal extension
 */
EJDB_STATIC bool _initintext(HANDLE fd, tcomode_t omode, uint32_t *apphdrsz, BPOPTS *opts, void *opaque) {
    assert(opaque);
    BPOOL *bp = opaque;
    BPEXT *e = bp->ext; //Master extension
    assert(e);
    opts->bpow = e->bpow;
    opts->ppow = e->ppow;
    opts->maxsize = e->maxsize;
    opts->incpow = e->incpow;
    *apphdrsz = 0;
    return true;
}

EJDB_INLINE size_t _extdataoff(BPEXT *ext) {
    return ext->apphdrsz + BPHDRSIZ + /* header */ + FBSIZE(ext) /*blocks bitmap */ + 1 /* term NULL */;
}

//compare function for BPEXT->lpages tre
EJDB_STATIC int _extpagescmp(const char *aptr, int asiz, const char *bptr, int bsiz, void *op) {
    BPEXT *ext = op;
    assert(ext);
    LPAGE *lp1 = (LPAGE*) aptr;
    LPAGE *lp2 = (LPAGE*) bptr;
    assert(lp1 && lp2);
    return (lp1->id < lp2->id ? -1 : (lp1->id > lp2->id ? 1 : 0));
}

/**
 * Lock specific page within extent.
 * @param ext Extent.
 * @param pnum Page ID.
 * @param wf If true the page will be exclusively locked for writing.
 */
EJDB_STATIC int _extpnumlock(BPEXT *ext, int pnum, bool wr) {
    if (!ext->lpagesmtx) {
        return TCESUCCESS;
    }
    int rv = pthread_mutex_lock(ext->lpagesmtx);
    if (rv) return rv;
    int sp;
    LPAGE *lp;
start:
    lp = (LPAGE*) tctreeget(ext->lpages, &pnum, sizeof(pnum), &sp);
    if (lp == NULL) {
        LPAGE lps = {
            .id = pnum,
            .refs = 0,
            .cv = PTHREAD_COND_INITIALIZER
        };
        tctreeput(ext->lpages, &pnum, sizeof(pnum), &lps, sizeof(lps));
        lp = &lps;
    }
    if (lp->refs == INT32_MAX /*someone writing*/ ||  (lp->refs > 0 && wr) /*we want to write but have a reades */) {
        //wait on condvar
        ++ lp->wrefs;
        rv = pthread_cond_wait(&(lp->cv), ext->lpagesmtx);
        -- lp->wrefs;
        if (rv) {
            goto finish;
        } else {
            goto start;
        }
    }
    if (wr) {
        assert(lp->refs == 0);
        lp->refs = INT32_MAX;
    } else {
        ++ lp->refs;
    }

finish:
    pthread_mutex_unlock(ext->lpagesmtx);
    return rv;
}

/**
 * Unlock specified page within extent.
 * @param ext Extent.
 * @param plum Page ID.
 */
EJDB_STATIC int _extpnumunlock(BPEXT *ext, int pnum) {
    if (!ext->lpagesmtx) {
        return TCESUCCESS;
    }
    int rv = pthread_mutex_lock(ext->lpagesmtx);
    if (rv) return rv;
    int sp;
    LPAGE *lp = (LPAGE*) tctreeget(ext->lpages, &pnum, sizeof(pnum), &sp);
    if (lp == NULL) {
        return TCBPEUNBALANCEDPL;
    }
    bool wr = (lp->refs == INT32_MAX);
    if (wr) {
        lp->refs = 0;
    } else if (lp->refs > 0) {
        -- lp->refs;
    } else {
        rv = TCBPEUNBALANCEDPL;
    }
    if (rv) {
        //tctreeout(ext->lpages, &pnum, sizeof(pnum)); //recovering & cleanup
    } else if (lp->wrefs > 0) {
        rv = wr ? pthread_cond_broadcast(&(lp->cv)) : pthread_cond_signal(&(lp->cv));
    } else if (lp->refs < 1) {
        tctreeout(ext->lpages, &pnum, sizeof(pnum));
    }
    pthread_mutex_unlock(ext->lpagesmtx);
    return rv;
}

/**
 * Lock extent area
 * @param ext BPEXT
 * @param off Global address offset
 * @param len Locked area length
 * @param wr If true area will be exclusively locked for writing
 */
EJDB_STATIC int _extplock(BPEXT *ext, int64_t off, size_t len, bool wr) {
    if (!ext->lpagesmtx) {
        return TCESUCCESS;
    }
    int rv = TCESUCCESS;
    int64_t eoff = off - ext->goff;
    assert(eoff >= 0);
    off_t spn = (int64_t) ROUNDUP(eoff, PSIZE(ext)) / PSIZE(ext);
    off_t epn = (int64_t) ROUNDUP(eoff + len, PSIZE(ext)) / PSIZE(ext);
    for (off_t i = spn; i <= epn; ++i) {
        int frv = _extpnumlock(ext, i, wr);
        if (frv) {
            rv = frv;
        }
    }
    return rv;
}

EJDB_STATIC int _extpunlock(BPEXT *ext, int64_t off, size_t len) {
    if (!ext->lpagesmtx) {
        return TCESUCCESS;
    }
    int rv = TCESUCCESS;
    int64_t eoff = off - ext->goff;
    assert(eoff >= 0);
    off_t spn = (int64_t) ROUNDUP(eoff, PSIZE(ext)) / PSIZE(ext);
    off_t epn = (int64_t) ROUNDUP(eoff + len, PSIZE(ext)) / PSIZE(ext);
    for (off_t i = spn; i <= epn; ++i) {
        int frv = _extpnumunlock(ext, i);
        if (frv) {
            rv = frv;
        }
    }
    return rv;
}

EJDB_STATIC int _bpsync(BPOOL *bp) {
    int rv = TCESUCCESS;
    return rv;
}

EJDB_STATIC int _loadmeta(BPEXT *ext, const char *buf) {
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

    memcpy(&(ext->ppow), buf + rp, sizeof(ext->ppow));
    rp += sizeof(ext->ppow);
    if (ext->ppow == 0 || ext->ppow > BPPPOWMAX) {
        ext->fatalcode = TCEMETA;
        return TCEMETA;
    }
    
    memcpy(&(ext->incpow), buf + rp, sizeof(ext->incpow));
    rp += sizeof(ext->incpow);
    if (ext->incpow > BPPOWINCMAX) {
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

EJDB_INLINE int _extsyncmeta(BPEXT *ext) {
    assert(ext && ext->hdrmmap.data);
    int rv = _extdumpmeta2(ext);
    return (rv == 0 ? tcmsync(0, BPHDRSIZ, true) : rv);
}

/**
 * Dump extent meta directly into extent header.
 */
EJDB_STATIC int _extdumpmeta2(BPEXT *ext) {
    assert(ext && ext->hdrmmap.data);
    char h[BPHDRSIZ];
    int rv = _extdumpmeta(ext, h);
    if (rv) return rv;
    memcpy(ext->hdrmmap.data + ext->apphdrsz, h, BPHDRSIZ);
    return rv;
}

/**
 * Dump extent meta into the specified buffer.
 * Buffer size must be >=  (BPHDRSIZ - BPHDREXTRAOFF)
 */
EJDB_STATIC int _extdumpmeta(BPEXT *ext, char *buf) {
    memset(buf, 0, BPHDRSIZ - BPHDREXTRAOFF);
    int wp = 0;
    uint16_t snum;
    snum = BPHDRMAGIC;
    snum = TCHTOIS(snum);
    memcpy(buf, &snum, sizeof(snum));
    wp += sizeof(snum);

    int64_t llnum;
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

    memcpy(buf + wp, &(ext->ppow), sizeof(ext->ppow));
    wp += sizeof(ext->ppow);

    memcpy(buf + wp, &(ext->incpow), sizeof(ext->incpow));
    wp += sizeof(ext->incpow);

    ext->nextext = (ext->next != NULL) ? 0x01 : 0x00;
    memcpy(buf + wp, &(ext->nextext), sizeof(ext->nextext));
    return TCESUCCESS;
}

EJDB_STATIC int _extsetmtx(BPEXT *ext) {
    int rv = TCESUCCESS;
    TCMALLOC(ext->lpagesmtx, sizeof(pthread_mutex_t));
    rv = pthread_mutex_init(ext->lpagesmtx, NULL);
    if (rv) {
        goto finish;
    }
    TCMALLOC(ext->fblocksmtx, sizeof(pthread_mutex_t));
    rv = pthread_mutex_init(ext->fblocksmtx, NULL);
    ext->lpages = tctreenew2(_extpagescmp, ext);
finish:
    return rv;
}

EJDB_STATIC int _extnew(BPOOL *bp, BPEXT** ext) {
    int rv = TCESUCCESS;
    BPEXT *e;
    TCMALLOC(e, sizeof (*e));
    memset(e, 0, sizeof (*e));
    if (bp->mmtx) {
        rv = _extsetmtx(e);
    }
    if (rv) {
        _extdel(e);
        e = NULL;
    }
    e->bp = bp;
    *ext = e;
    return rv;
}

EJDB_STATIC int _extclose(BPEXT *ext) {
    assert(ext);
    if (!INVALIDHANDLE(ext->fd)) {
        CLOSEFH(ext->fd);
    }
    return TCESUCCESS;
}

EJDB_STATIC int _extdel(BPEXT *ext) {
    assert(ext);
    if (ext->lpages) {
        tctreedel(ext->lpages);
    }
    tcunmap(&(ext->hdrmmap));
    if (ext->lpagesmtx) {
        pthread_mutex_destroy(ext->lpagesmtx);
        TCFREE(ext->lpagesmtx);
    }
    if (ext->fblocksmtx) {
        pthread_mutex_destroy(ext->fblocksmtx);
        TCFREE(ext->fblocksmtx);
    }
    if (ext->fpath) {
        TCFREE(ext->fpath);
    }

    TCFREE(ext);
    return TCESUCCESS;
}

EJDB_STATIC int _extopen(BPEXT *ext, const char *fpath, tcomode_t omode, TCBPINIT init, void *initop) {
    assert(ext && ext->bp);
    int rv = TCESUCCESS;
    char hbuf[BPHDRSIZ]; //BPE header buffer
    BPOPTS opts = {0};
    struct stat sbuf; //BPE file stat buff

    ext->fpath = strdup(fpath);
#ifndef _WIN32
    int mode = O_RDONLY;
    if (omode & TCOWRITER) {
        mode = O_RDWR;
        if (omode & TCOCREAT) mode |= O_CREAT;
        if (omode & O_TRUNC) mode |= O_TRUNC;
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
        //typedef bool (*TCBPINIT) (HANDLE fd, tcomode_t omode, uint32_t *apphdrsz, BPOPTS *opts, void *opaque);
        if (!init(ext->fd, omode, &(ext->apphdrsz), &opts, initop)) {
            rv = TCBPEXTINIT;
            goto finish;
        }
    }
    if (ext->apphdrsz > 0 && !tcfseek(ext->fd, ext->apphdrsz, TCFSTART)) {
        rv = TCESEEK;
        goto finish;
    }
    if (fstat(ext->fd, &sbuf)) {
        rv = TCESTAT;
        goto finish;
    }
    if (sbuf.st_size > ext->apphdrsz && !(omode & TCOTRUNC)) { //trying to read the existing header
        if (!tcread(ext->fd, hbuf, BPHDRSIZ)) {
            rv = TCEREAD;
            goto finish;
        }
        rv = _loadmeta(ext, hbuf);
        if (rv) {
            goto finish;
        }
        size_t ef = _extdataoff(ext);
        if (sbuf.st_size < ef) {
            rv = TCBPEINVFB;
            goto finish;
        }
    } else { //init new meta
        if (omode & BPODEBUG) {
            ext->ppow = (opts.ppow > 0) ? opts.ppow : BPPPOWDEF;
            ext->bpow = (opts.bpow > 0) ? opts.bpow : BPBPOWDEF;
            ext->maxsize = (opts.maxsize > 0 ? opts.maxsize : BPDEFMAXSIZE);
            ext->incpow = (opts.incpow > 0 ? opts.incpow : BPPOWINCDEF); 
            
        } else {
            ext->ppow = (opts.ppow >= BPPPOWMIN && opts.ppow <= BPPPOWMAX) ? opts.ppow : BPPPOWDEF;
            ext->bpow = (opts.bpow > 0 && opts.bpow <= BPBPOWMAX) ? opts.bpow : BPBPOWDEF;
            ext->maxsize = (opts.maxsize > 0 ? (opts.maxsize < BPEXTMINSIZE ? BPEXTMINSIZE : opts.maxsize) : BPDEFMAXSIZE);
            ext->incpow = (opts.incpow > 0 ? (opts.incpow > BPPOWINCMAX ? BPPOWINCMAX : opts.incpow) : BPPOWINCDEF);
        }
        ext->maxsize = ROUNDUP(ext->maxsize, PSIZE(ext));
        assert(!ext->size);
        assert(!ext->nextext);

        if (ext->ppow <= ext->bpow || (ext->maxsize <= (1 << ext->ppow))) {
            rv = TCBPEOPTS;
            goto finish;
        }
        rv = _extdumpmeta(ext, hbuf);
        if (rv) {
            goto finish;
        }
        if (!tcwrite(ext->fd, hbuf, BPHDRSIZ)) {
            rv = TCEWRITE;
            goto finish;
        }
        //fill free-block bitmap by zeros
        int bb = FBSIZE(ext) + 1 /*NULL*/;
        char zb[BPIOBUFSIZ];
        memset(zb, 0, BPIOBUFSIZ);
        while (bb > 0) {
            int w = MIN(bb, BPIOBUFSIZ);
            if (!tcwrite(ext->fd, zb, w)) {
                rv = TCEWRITE;
                break;
            }
            bb -= w;
        }
    }
    TCMMAP *hmm = &(ext->hdrmmap);
    memset(hmm, 0, sizeof(*hmm));
    hmm->fd = ext->fd;
    hmm->len = _extdataoff(ext);
    hmm->off = 0;
    rv = tcmmap(hmm); //mmap custom app headers and fb bitmap
    
finish:
    if (rv) { //error code
        if (!INVALIDHANDLE(ext->fd)) {
            CLOSEFH(ext->fd);
        }
        ext->fatalcode = rv;
    }

    return rv;
}

EJDB_STATIC int _extensurend(BPEXT *ext, int64_t end) {
    int rv = TCESUCCESS;
    return rv;
}

EJDB_STATIC int _extcreate(BPEXT *prev, BPEXT *next, int64_t end) {
    int rv = TCESUCCESS;
    return rv;
}

EJDB_STATIC int _extselect(BPOOL *bp, BPEXT **ext, int64_t off, size_t len, bool wr) {
    assert(bp && ext && bp->ext);
    int rv = BPLOCKEXT(bp);
    if (rv) return rv;
    int64_t end = off + len;
    BPEXT *e = bp->ext;
    if (!e) {
        return TCBPEXTNOTFOUND;
    }
    if (!wr) { //readonly
        int en = (end / (e->maxsize + 1)) + 1;
        while (e && --en > 0) e = e->next;
        *ext = e;
        if (e == NULL) {
            BPUNLOCKEXT(bp);
            return TCBPEXTNOTFOUND;
        }
        BPUNLOCKEXT(bp);
        return rv;
    } else {
        int en = (end / (e->maxsize + 1)) + 1;
        BPEXT *laste = e;
        int ec = 1; // ext counter
        while (e && --en > 0) {
            laste = e;
            e = e->next;
            ++ec;
        }
        for (int i = 1; i <= en; ++i, ++ec) {
            BPEXT *next;
            rv = _extnew(bp, &next);
            if (rv) goto finish;
            if (bp->mmtx) {
                rv = _extsetmtx(next);
                if (rv) {
                    goto finish;
                }
            }
            char *epath = tcsprintf("%s.%d", e->fpath, en);
            rv = _extopen(next, epath, (bp->omode | TCOTRUNC), _initintext, bp);
            TCFREE(epath);
            if (rv) {
                goto finish;
            }
            if (i < en) { //full extent allocation
                if (!tcfensurespace(next->fd, e->maxsize, 0, e->maxsize, &(next->size))) {
                    rv = TCEWRITE;
                    goto finish;
                }
            }
            laste->nextext = 0x01;
            laste->next = next;
            if (i < en) {
                rv = _extsyncmeta(laste);
                if (rv) {
                    goto finish;
                }
            }
            laste = next;
        }
        assert(laste);
        assert(ec * e->maxsize >= end);
        if (!tcfensurespace(laste->fd, (ec * e->maxsize - end), INCSIZE(laste), e->maxsize, &(laste->size))) {
            rv = TCEWRITE;
        } else {
            laste->next = NULL;
            laste->nextext = 0x00;
            rv = _extsyncmeta(laste);
        }
    }
finish:
    BPUNLOCKEXT(bp);
    return rv;
}

EJDB_INLINE int _bplockmeth(BPOOL *bp, bool wr) {
    return (wr ? pthread_rwlock_wrlock(bp->mmtx) :  pthread_rwlock_rdlock(bp->mmtx));
}

EJDB_INLINE int _bpunlockmeth(BPOOL *bp) {
    return pthread_rwlock_unlock(bp->mmtx);
}

EJDB_INLINE int _bplockext(BPOOL *bp) {
    return pthread_mutex_lock(bp->extmtx);
}

EJDB_INLINE int _bpunlockext(BPOOL *bp) {
    return pthread_mutex_unlock(bp->extmtx);
}
