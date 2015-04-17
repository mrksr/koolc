#!/bin/zsh

REFC="scala -cp ./reference/koolc_2.11-1.2.jar:./reference/cafebabe_2.11-1.2.jar koolc.Main"
MYC="scala -cp ./target/scala-2.11/koolc_2.11-1.0.jar koolc.Main"
TESTDIR="./testprograms/lab3/valid"

# for p in $TESTDIR/*; do
#     echo "*************************"
#     echo "* Testing: $p"
#     ${=REFC} --tokens $p > /tmp/reftokens.koolc
#     ${=MYC}  --tokens $p > /tmp/mytokens.koolc
#     diff -q /tmp/reftokens.koolc /tmp/mytokens.koolc
# done

for p in $TESTDIR/*; do
    echo "*************************"
    echo "* Testing AST: $p"
    ${=REFC} --ast $p               > /tmp/refast.koolc
    ${=MYC}        $p               > /tmp/myast.koolc
    ${=REFC} --ast /tmp/myast.koolc > /tmp/normast.koolc
    diff -q /tmp/refast.koolc /tmp/normast.koolc
done

