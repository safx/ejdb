/**
 * Test cases for buffer pool  implementation.
 */

#include "myconf.h"
#include "tcbpool.h"
#include <locale.h>
#include <CUnit/Basic.h>


/* get a random number */
static int myrand(int range) {
    if (range < 2) return 0;
    int high = (unsigned int) rand() >> 4;
    int low = range * (rand() / (RAND_MAX + 1.0));
    low &= (unsigned int) INT_MAX >> 4;
    return (high + low) % range;
}

typedef struct {
    BPOPTS opts;
    void *hdr;
    uint32_t hdrsz;
} _BPINIT;


static bool customBPInitFunc(HANDLE fd, tcomode_t omode, uint32_t *hdrsiz, BPOPTS *opts, void *opaque) {
    int rv = true;
    _BPINIT *bpi = opaque;
    CU_ASSERT_PTR_NOT_NULL_FATAL(bpi);
    CU_ASSERT_PTR_NOT_NULL_FATAL(opts);
    assert(bpi && opts);
    memcpy(opts, &(bpi->opts), sizeof(*opts));
    if (bpi->hdr && bpi->hdrsz > 0) {
        if (omode & TCOWRITER) {
            rv = tcwrite(fd, bpi->hdr, bpi->hdrsz);
            CU_ASSERT_TRUE_FATAL(rv);
        }
    }
    *hdrsiz = bpi->hdrsz;
    return rv;
}


//typedef bool (*TCBPINIT) (HANDLE fd, tcomode_t omode, uint32_t *hdrsiz, BPOPTS *opts, void *opaque);
static bool initTestOpenClose(HANDLE fd, tcomode_t omode, uint32_t *hdrsiz, BPOPTS *opts, void *opaque) {
    bool rv = true;
    int64_t h = 0xfffa11fffcfd8ff1LL;
    h = TCHTOILL(h);
    rv = tcwrite(fd, &h, sizeof(h));
    *hdrsiz = sizeof(h);
    return rv;
}

int init_suite(void) {
    srand(tctime() * 1000);
    return 0;
}

int clean_suite(void) {
    return 0;
}

static void _testOpenClose(const char *fn, int omode, TCBPINIT init) {
    BPOOL *bp;
    CU_ASSERT_PTR_NOT_NULL_FATAL(fn);
    int rv = tcbpnew(&bp);
    CU_ASSERT_EQUAL(rv, 0);
    CU_ASSERT_PTR_NOT_NULL_FATAL(bp);
    rv = tcbpsetmtx(bp);
    CU_ASSERT_EQUAL(rv, 0);
    rv = tcbpopen(bp, fn, omode, init, NULL);
    CU_ASSERT_EQUAL(rv, 0);
    int64_t h;
    CU_ASSERT_EQUAL(tcbpapphdrsiz(bp), sizeof(h));
    int len = tcbpapphdread(bp, &h, 0, sizeof(h));
    h = TCITOHLL(h);
    CU_ASSERT_EQUAL(len, sizeof(h));
    rv = tcbpclose(bp);
    CU_ASSERT_EQUAL(rv, 0);
    tcbpdel(bp);
}

void testOpenClose() {
    _testOpenClose("bp1", BPDEFOMODE | TCOTRUNC, initTestOpenClose);
}

void testOpenClose2() {
    _testOpenClose("bp1", 0, initTestOpenClose);
}

static int _testRandomRW(const char *fn, int omode, int hsz, _BPINIT *bpi) {
    omode |= BPODEBUG;
    CU_ASSERT_PTR_NOT_NULL_FATAL(bpi);
    assert(bpi);
    char *heap;
    BPOOL *bp;
    TCCALLOC(heap, 1, hsz);
    int rv = tcbpnew(&bp);
    CU_ASSERT_EQUAL(rv, 0);
    CU_ASSERT_PTR_NOT_NULL_FATAL(bp);
    rv = tcbpsetmtx(bp);
    CU_ASSERT_EQUAL(rv, 0);

    rv = tcbpopen(bp, fn, omode, customBPInitFunc, bpi);
    CU_ASSERT_EQUAL(rv, 0);
    
    myrand(10); 

    rv = tcbpclose(bp);
    CU_ASSERT_EQUAL(rv, 0);
    tcbpdel(bp);
    TCFREE(heap);
    return rv;
}

void testRandomRW() {
    uint64_t h = 0xc0c07e7d6ba3;
    _BPINIT bpi = {
        .opts = {
            .ppow = 8, //256
            .bpow = 6, //64
            .maxsize = 1024 //1K
        },
        .hdr = &h,
        .hdrsz = sizeof(h)
     };
    int rv = _testRandomRW("bp1", (BPDEFOMODE | TCOTRUNC), 1024 * 1024, &bpi);
    CU_ASSERT_EQUAL(rv, 0);
}

//void testFullRE



int main() {
    setlocale(LC_ALL, "en_US.UTF-8");
    CU_pSuite pSuite = NULL;

    /* Initialize the CUnit test registry */
    if (CUE_SUCCESS != CU_initialize_registry())
        return CU_get_error();

    /* Add a suite to the registry */
    pSuite = CU_add_suite("t0", init_suite, clean_suite);
    if (NULL == pSuite) {
        CU_cleanup_registry();
        return CU_get_error();
    }

    /* Add the tests to the suite */
    if (
        //(NULL == CU_add_test(pSuite, "BP:testOpenClose", testOpenClose)) ||
        //(NULL == CU_add_test(pSuite, "BP:testOpenClose2", testOpenClose2)) ||
        (NULL == CU_add_test(pSuite, "BP:testRandomRW", testRandomRW))
    ) {
        CU_cleanup_registry();
        return CU_get_error();
    }

    /* Run all tests using the CUnit Basic interface */
    CU_basic_set_mode(CU_BRM_VERBOSE);
    CU_basic_run_tests();
    int ret = CU_get_error() || CU_get_number_of_failures();
    CU_cleanup_registry();
    return ret;
}
