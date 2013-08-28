/**
 * Test cases for buffer pool  implementation.
 */

#include "myconf.h"
#include "tcbpool.h"
#include <locale.h>
#include <CUnit/Basic.h>

//typedef bool (*TCBPINIT) (HANDLE fd, tcomode_t omode, uint32_t *hdrsiz, BPOPTS *opts, void *opaque);

bool initTestOpenClose(HANDLE fd, tcomode_t omode, uint32_t *hdrsiz, BPOPTS *opts, void *opaque) {
    bool rv = true;
    int64_t h = 0xfffa11fffcfd8ff1LL; 
    h = TCHTOILL(h);
    rv = tcwrite(fd, &h, sizeof(h));
    *hdrsiz = sizeof(h);    
    return rv;
}

int init_suite(void) {
    return 0;
}

int clean_suite(void) {
    return 0;
}

void _testOpenClose(const char *fn, int omode, TCBPINIT init) {
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
        (NULL == CU_add_test(pSuite, "BP:testOpenClose", testOpenClose)) || 
        (NULL == CU_add_test(pSuite, "BP:testOpenClose2", testOpenClose2)) 
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
