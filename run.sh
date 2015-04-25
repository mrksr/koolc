#!/bin/zsh

REFC="scala -cp ./reference/koolc_2.11-1.3.1.jar:./reference/cafebabe_2.11-1.2.jar koolc.Main"
MYC="scala -cp ./target/scala-2.11/koolc_2.11-1.0.jar:./lib/cafebabe_2.10-1.2.jar koolc.Main"
TESTDIR="./testprograms/lab3/valid"

# for p in $TESTDIR/*; do
#     echo "*************************"
#     echo "* Testing: $p"
#     ${=REFC} --tokens $p > /tmp/reftokens.koolc
#     ${=MYC}  --tokens $p > /tmp/mytokens.koolc
#     diff -q /tmp/reftokens.koolc /tmp/mytokens.koolc
# done

# for p in $TESTDIR/*; do
#     echo "*************************"
#     echo "* Testing AST: $p"
#     ${=REFC} --ast $p               > /tmp/refast.koolc
#     ${=MYC}        $p               > /tmp/myast.koolc
#     ${=REFC} --ast /tmp/myast.koolc > /tmp/normast.koolc
#     diff -q /tmp/refast.koolc /tmp/normast.koolc
# done

# for p in $TESTDIR/*; do
#     echo "*************************"
#     echo "* Testing symid: $p"
#     ${=REFC} --symid $p > /tmp/refsymid.koolc
# done

REFD=$(mktemp -d "/tmp/kool.ref.XXX")
OWND=$(mktemp -d "/tmp/kool.own.XXX")
REFO=$(mktemp "/tmp/kool.ref.out.XXX")
OWNO=$(mktemp "/tmp/kool.own.out.XXX")

for p in $TESTDIR/*; do
    echo "*************************"
    echo "* Testing output: $p"
    BASE=$(basename $p ".kool")
    ${=REFC} $p -d $REFD
    ${=MYC} $p -d $OWND
    java -cp $REFD $BASE > $REFO
    java -cp $OWND $BASE > $OWNO
    diff -q $REFO $OWNO
done
