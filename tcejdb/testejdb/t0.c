/**
 * Test cases for buffer pool  implementation.
 */

#include <CUnit/Basic.h>
#include "tcbpool.h"
#include <locale.h>

//typedef bool (*TCBPINIT) (HANDLE fd, tcomode_t omode, uint32_t *hdrsiz, BPOPTS *opts, void *opaque);

bool bpinit(HANDLE fd, tcomode_t omode, uint32_t *hdrsiz, BPOPTS *opts, void *opaque) {
    int rv = true;
    int64_t h = 0xfffa11fffcfd8ff1LL; 
    rv = tcwrite(fd, &h, sizeof(h));
    *hdrsiz = sizeof(h);
    //printf("\n\n%lu", sizeof(buf));
    return rv;
}

int init_suite(void) {
    return 0;
}

int clean_suite(void) {
    return 0;
}

void testOpenClose() {
    BPOOL *bp;
    int rv = tcbpnew(&bp);
    CU_ASSERT_EQUAL(rv, 0);
    CU_ASSERT_PTR_NOT_NULL_FATAL(bp);
    rv = tcbpopen(bp, "bp1", 0, bpinit, NULL);
    CU_ASSERT_EQUAL(rv, 0);
    rv = tcbpclose(bp);
    CU_ASSERT_EQUAL(rv, 0);
    tcbpdel(bp);
}

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
    if ((NULL == CU_add_test(pSuite, "testOpenCloseBP", testOpenClose))
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
