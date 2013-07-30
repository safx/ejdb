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

typedef enum { /** error codes */
    TCBPERONLY = 8001, /**< BP in readonly mode */
    TCBPEXTINIT = 8002 /**< BP extent initalization failed */
} bpret_t;

typedef enum {
    TCBPLOSED = 0, /**< BP in closed state */
    TCBPOPEN = 1 /**< BP in open state */
} bpstate_t;

typedef struct { /** BP options */
    uint8_t bpow; /**< The power of buffer aligment */
    uint64_t maxsize; /**< The maximum size of BP extent */
} BPOPTS;

struct BPOOL; /**< BPOOL object. */
typedef struct BPOOL BPOOL;

typedef bool (*TCBPINIT) (HANDLE fd, tcomode_t omode, uint32_t *hdrsiz, BPOPTS *opts, void *opaque);

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
int tcbpopen(BPOOL *bp, const char *fpath, tcomode_t omode, TCBPINIT init, void *initop);

/**
 * Closes buffer pool.
 * @param bp
 * @return
 */
int tcbpclose(BPOOL *bp);

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

