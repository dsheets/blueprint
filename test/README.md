# blueprint tests

These tests exercise the `blue` command line tool and success is defined
as byte-identical output to an expected result.

## The structure of a test

Each test consists of a shell script, `test.sh`, which is executed with
an environment including `OCAMLRUNPARAM=b` and `BLUE` set to the
specific invocation of the `blue` tool under test (e.g. `blue -i`). By
default, tests expect input in `in.xml`. The output of the test will be
compared against `out.xml` if the test script exits 0, `err.txt` if the
test script exits 1, or `trouble.txt` if the test script exits
2. Symbolic links are used to keep the test result variations in sync if
they are expected to be the same.

## Creating a new test

Use `cp -R new_success_tmpl my_new_success_test` or `cp -R
new_error_tmpl my_new_error_test` to create a new success or error test
respectively. Then, add this test to the test listing at the end of
`test.ml`.
