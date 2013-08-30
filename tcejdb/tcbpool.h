/**************************************************************************************************
 *  EJDB database library http://ejdb.org
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

#include "tcutil.h"

#ifndef TCBPOOL_H
#define	TCBPOOL_H

EJDB_EXTERN_C_START

#define BPDEFOMODE (TCOREADER | TCOWRITER | TCOCREAT)
#define BPODEBUG (1 << 30)

typedef enum { /** error codes */
    TCBPERONLY = 8001, /**< BP in readonly mode */
    TCBPEXTINIT = 8002, /**< BP extent initalization failed */
    TCBPECLOSED = 8003, /**< BP is closed already */
    TCBPEOPENED = 8004,  /**< BP is opened already */
    TCBPEADDRALIGN = 8005, /**< Unaligned BP address */
    TCBPEBLKOVERFLOW = 8006, /**< Requested block is greater than extent size */
    TCBPEXTNOTFOUND = 8007, /**< BP extent not found */
    TCBPEUNBALANCEDPL = 8008, /**< Unbalanced page locks */
    TCBPEOPTS = 8009 /**< Invalid BP options. */
} bpret_t;

typedef enum {
    TCBPLOSED = 0, /**< BP in closed state */
    TCBPOPEN = 1 /**< BP in open state */
} bpstate_t;

typedef struct { /**< BP options */
    uint8_t ppow; /**< Power of page size */
    uint8_t bpow; /**< Power of buffer aligment */
    int64_t maxsize; /**< The maximum size of BP extent */
    uint8_t incpow; /**< Extent increment delta pow */ 
} BPOPTS;

typedef struct { /**< BP info */
   uint32_t apphdrsz; /**< Size of custom application header in the first extent */
   uint8_t ppow; /**< Power of page size */
   uint8_t bpow;  /**< Power of buffer aligment */
   int64_t maxsize; /**< Maximum size of whole buffer pool */
   int maxcachepages; /**< Maximum number of cached pages */
} BPINFO;

struct PAGE;
typedef struct PAGE PAGE; /**< Page handle */

struct BPOOL; /**< BPOOL object. */
typedef struct BPOOL BPOOL;

struct BPEXT; /**< BPEXT object. */
typedef struct BPEXT BPEXT;

struct LPAGE;
typedef struct LPAGE LPAGE;

typedef bool (*TCBPINIT) (HANDLE fd, tcomode_t omode, uint32_t *hdrsiz, BPOPTS *opts, void *opaque);

/** 
 * Creates new zero initalized `TCBPOOL` structure instance.
 */
int tcbpnew(BPOOL** bp);

/**
 * Set BP mutexes in the case of concurrent access.
 */
int tcbpsetmtx(BPOOL *bp);

/**
 * Return size of custom app header size in the first extent.
 */
int tcbpapphdrsiz(BPOOL *bp);

/**
 * Write custom application headre into `buf`.
 * @param bp Buffer pool.
 * @param buf Target buffer.
 * @param off Offset in the header data.
 * @param len Number of bytes to write.
 * @return Size actually read.
 */
int tcbpapphdread(BPOOL *bp, void *buf, int off, int len);

int tcbpapphdwrite(BPOOL *bp, int hoff, char *buf, int boff, int len);

/**
 * Opens buffer pool.
 * @param bp
 * @param fname
 * @param omode
 * @param init
 * @return Error code.
 */
int tcbpopen(BPOOL *bp, const char *fpath, tcomode_t omode, TCBPINIT init, void *initop);

/**
 * Closes buffer pool.
 * @param bp
 * @return Error code.
 */
int tcbpclose(BPOOL *bp);

/**
 * Sync buffer pool with disk
 * @param bp
 * @return Error code.
 */
int tcbpsync(BPOOL *bp);

/**
 * Returns true if passed BP in open state.
 */
bool tcbpisopen(BPOOL *bp);

/**
 * Deletes the `TCBPOOL` structure. If underlying buffer pool is open
 * it will be closed explicitly by `tcbclose`.
 * @param bp
 */
void tcbpdel(BPOOL *bp);

/**
 * Lock pages in BP.
 * @param bp Buffer pool.
 * @param ext Optional output pointer into which first offset matching extent will be saved.
 * @param off First block address.
 * @param len Number of bytes to lock.
 * @param wr If true an exclusive write lock will be acquired on pages.
 * @return Error code.
 */
int tcbplock(BPOOL *bp, BPEXT **ext, int64_t off, size_t len, bool wr);

/**
 * Unlock page in BP.
 * @return Error code.
 */
int tcbpunlock(BPOOL *bp, int64_t off, size_t len);

/**
 * Read BP data.
 * In multithreaded environment every call of `tcbpread` must be guarded by overlapping `tcbplock`.
 * @param bp Buffer pool.
 * @param off Read address offset.
 * @param len Number of bytes to read.
 * @param out Buffer to write data.
 * @param sp Output pointer into which number of bytes actually read will be saved.
 * @return Error code.
 */
int tcbpread(BPOOL *bp, int64_t off, size_t len, char *out, size_t *sp);

/**
 * Read BP data.
 * This call is protected by matching `tcbplock`.
 * @param bp Buffer pool.
 * @param off Read address offset.
 * @param len Number of bytes to read.
 * @param out Buffer to write data.
 * @param sp Output pointer into which number of bytes actually read will be saved.
 * @return Error code.
 */
int tcbplockread(BPOOL *bp, int64_t off, size_t len, char *out, size_t *sp);

/**
 * Write BP data.
 * In multithreaded environment every call of `tcbpwrite` must be guarded by overlapping `tcbplock`.
 * @param bp Buffer pool.
 * @param off Write address offset.
 * @param buf Input buffer to write.
 * @param len Number of bytes to write.
 * @param sp Output pointer into which number of bytes actually written will be saved.
 * @return Error code.
 */
int tcbpwrite(BPOOL *bp, int64_t off, const void *buf, size_t len, size_t *sp);

/**
 * Write BP data.
 * This call is protected by matching `tcbplock`.
 * @param bp Buffer pool.
 * @param off Write address offset.
 * @param buf Input buffer to write.
 * @param len Number of bytes to write.
 * @param sp Output pointer into which number of bytes actually written will be saved.
 * @return Error code.
 */
int tcbplockwrite(BPOOL *bp, int64_t off, const void *buf, size_t len, size_t *sp);


/**
 * Fetch BP meta-info.
 * @param bp Buffer pool.
 * @param bpi Buffer pool info placeholder.
 * @return Error code.
 */
int tcbpinfo(BPOOL *bp, BPINFO *bpi);


EJDB_EXTERN_C_END

#endif	/* TCBPOOL_H */
