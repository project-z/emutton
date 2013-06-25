#!/bin/sh

set -e

test `basename $PWD` != "c_src" && cd c_src

case "$1" in
  clean)
    rm -rf mutton/build/lib/*
    ;;

  *)
    test -f mutton/build/lib/libmutton.* && exit 0

    (test -d mutton || git clone git@github.com:project-z/mutton.git mutton)

    #VERBOSE=1 cmake -DCMAKE_BUILD_TYPE=debug -DCMAKE_CXX_FLAGS="-fPIC -m$ERLANG_ARCH" .&& VERBOSE=1 make all mutton_test && ./test/unit/mutton_test
    #CXXFLAGS="-Wall -O3 -fPIC -pthread -m$ERLANG_ARCH"
    #CXX="${CXX:-c++} -m$ERLANG_ARCH"
    # this used to be included in the ``make all`` from
    # -j2 CXX="$CXX" CXXFLAGS="$CXXFLAGS"
    which gmake 1>/dev/null 2>/dev/null && MAKE=gmake
    MAKE=${MAKE:-make}
    (cd mutton && \
        cmake -DCMAKE_BUILD_TYPE=debug -DCMAKE_CXX_FLAGS="-fPIC -m$ERLANG_ARCH" . \
        && make all mutton_test \
        && ./test/unit/mutton_test)
    ;;
esac
