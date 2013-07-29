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

static bpret_t _openext(BPEXT *ext, const char *fpath, tcomode_t omode, TCBPINIT init, void *initop);
static bpret_t _closext(BPEXT *ext);
static BPEXT* _creatext();

BPOOL* tcbpnew() {
    BPOOL *ret;
    TCMALLOC(ret, sizeof (*ret));
    memset(ret, 0, sizeof (*ret));
    return ret;
}

bool tcbpisopen(BPOOL *bp) {
    return false;
}

void tcbpdel(BPOOL *bp) {
    assert(bp);
    if (tcbpisopen(bp)) {
        tcbpclose(bp);
    }
    TCFREE(bp);
}

bpret_t tcbpopen(BPOOL *bp, const char *fpath, tcomode_t omode, TCBPINIT init, void *initop) {
    assert(bp && init);
    bpret_t rv = TCBPOK;
    if (omode == 0) {
        omode = TCOREADER | TCOWRITER | TCOCREAT;
    }
    //Initialize main extent
    bp->ext = _creatext();
    rv = _openext(bp->ext, fpath, omode, init, initop);
    if (rv) {
        goto finish;

    }

finish:
    return rv;
}

bpret_t tcbpclose(BPOOL *bp) {
    return TCBPOK;
}

///////////////////////////////////////////////////////////////////////////
//                          Private staff
///////////////////////////////////////////////////////////////////////////

static BPEXT* _creatext() {
    BPEXT *ext;
    TCMALLOC(ext, sizeof (*ext));
    memset(ext, 0, sizeof (*ext));
    return ext;
}

static bpret_t _closext(BPEXT *ext) {
    assert(ext);
    //todo
}

static bpret_t _openext(BPEXT *ext, const char *fpath, tcomode_t omode, TCBPINIT init, void *initop) {
    HANDLE fd;
    bpret_t rv = TCBPOK;
    BPOPTS opts;

#ifndef _WIN32
    int mode = O_RDONLY;
    if (omode & TCOWRITER ) {
        mode = O_RDWR;
        if (omode & TCOCREAT) mode |= O_CREAT;
    }
    fd = open(fpath, mode, BPFILEMODE);
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
    if (INVALIDHANDLE(fd)) {
        return TCBPEOPEN;
    }

    if (init) {
        //typedef bool (*TCBPINIT) (HANDLE fd, tcomode_t omode, uint32_t *hdrsiz, BPOPTS *opts, void *opaque);
        uint32_t hdrsiz = 0;
        //if (!init(fd, omode, &hdrsiz, opts, ))

    } else {

    }

finish:

    return rv;
}
