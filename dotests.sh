
#step 0
#./runtest.py tests/step0_repl.mal ./run 2> ./testerror0

#step 1
./runtest.py tests/step1_read_print.mal ./run 2> ./testerror1

# ./runtest.py tests/step2_eval.mal ./run 2> ./testerror2

# ./runtest.py tests/step3_env.mal ./run 2> ./testerror3

# ./runtest.py tests/step4_if_fn_do.mal ./run 2> ./testerror4

# ./runtest.py tests/step5_tco.mal ./run 2> ./testerror5

# ./runtest.py tests/step6_file.mal ./run 2> ./testerror6

# ./runtest.py tests/step7_quote.mal ./run 2> ./testerror7

# ./runtest.py tests/step8_macros.mal ./run 2> ./testerror8

# ./runtest.py tests/step9_try.mal ./run 2> ./testerror9

# ./runtest.py tests/stepA_mal.mal ./run 2> ./testerrorA

cat ./testerror*

rm ./testerror*