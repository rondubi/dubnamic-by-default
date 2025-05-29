#!/bin/bash

set -v

dubnamic_path=$(dirname $0)
dubnamic_path=$(cd $dubnamic_path; pwd)
echo $dubnamic_path

filename=$1
echo "$1"
stem="${filename##*/}"
stem="${stem%.c}"
echo "$stem"

ABSOLUTE_FILE_NAME="$dubnamic_path/tests/$stem.c"
CC_NAME="$dubnamic_path/test-build/$stem.cc"
O_NAME="$dubnamic_path/test-build/$stem.o"
COMPILED_NAME="$dubnamic_path/test-build/$stem"
echo $COMPILED_NAME

cd $dubnamic_path/build
touch $CC_NAME
./dynamic-by-default $ABSOLUTE_FILE_NAME > $CC_NAME

clang++ -c -Wall -Wpedantic -Wno-deprecated-declarations -std=c++2a -I $BUILDIT_PATH/include -o $O_NAME $CC_NAME

clang++ -ldl $O_NAME $BUILDIT_PATH/build/libbuildit.a -o $COMPILED_NAME

cd $dubnamic_path

