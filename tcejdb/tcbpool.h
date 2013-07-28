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

#include "basedefs.h"
#include "stdint.h"
#include "stdbool.h"


#ifndef TCBPOOL_H
#define	TCBPOOL_H

EJDB_EXTERN_C_START

typedef enum { /** error codes */
    TCBPOK = 0,
    TCBPEOPEN = 8000, /**< BP file open error */
    TCBPERONLY = 8001, /**< BP in readonly mode */
    TCBPEXTINIT = 8002 /**< BP extent initalization failed */
} bpret_t;

typedef enum {
    TCBPOREAD = 1, /**< read open mode */
    TCBPOWRITE = 1 << 1, /**< write open mode */
    TCBPOCREATE = 1 << 2, /**< witer create */
    TCBONOLOCK = 1 << 3, /**< open in nolock mode */
    TCBONOBLK = 1 << 4, /**< open in noblock mode */
    TCBOTRUNC = 1 << 5 /**< truncate BP */
} bpomode_t;

typedef enum {
    TCBPLOSED = 0, /**< BP in closed state */
    TCBPOPEN = 1  /**< BP in open state */
} bpstate_t;

typedef struct { /** BP options */
    bpomode_t omode; /**< The buffer files open mode */
    uint8_t bpow; /**< The power of buffer aligment */
    uint64_t umaxsiz; /**< The maximum size of BP extent */
} BPOPTS;


typedef struct { /** BP extent */
    char *fpath; /**<Path to first BP extent */
    HANDLE *fd; /**< Extent file handle */
    uint32_t hdrsiz; /**< Size of custom app header */
    struct BPEXT *next; /**< Next BP extent */
} BPEXT;

typedef struct { /** BP itself */
    BPOPTS opts; /**< BP options */
    bpstate_t state; /**< BP state */
    BPEXT *ext; /**< First BP extent */
} BPOOL;

typedef bool (*TCBPINIT) (HANDLE fd, bpomode_t omode, uint32_t *hdrsiz, BPOPTS *opts, void *opaque);

/**
 * Creates new zero initalized `TCBPOOL` structure instance.
 */
BPOOL* tcbpnew();

/**
 * Opens buffer pool.
 * @param bp
 * @param fname
 * @param omode
 * @param init
 * @return
 */
bpret_t tcbpopen(BPOOL *bp, const char *fpath, bpomode_t omode, TCBPINIT init, void *initop);

/**
 * Closes buffer pool.
 * @param bp
 * @return
 */
bpret_t tcbpclose(BPOOL *bp);

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



EJDB_EXTERN_C_END

#endif	/* TCBPOOL_H */

