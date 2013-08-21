/**
 * Test cases for buffer pool  implementation.
 */

#include <CUnit/Basic.h>
#include "tcbpool.h"
#include <locale.h>

int init_suite(void) {   
    return 0;
}

int clean_suite(void) {    
    return 0;
}

void testOpenClose() {
    BPOOL *bp = tcbpnew();
    CU_ASSERT_PTR_NOT_NULL_FATAL(bp);
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
