
#step 0
STEP=step0_repl python ./runtest.py tests/step0_repl.mal ./run 2> ./testerror0

#step 1
STEP=step1_read_print python ./runtest.py tests/step1_read_print.mal ./run 2> ./testerror1

#step 2


cat ./testerror0
cat ./testerror1
