########################################################################
# cmutton - 7/8/2013 (guilty-party: lenards)
########################################################################
#
# CFLAGS Notes:
#
#   -Wno-gnu is to suppress the warnings about the __VA_ARGS__ in dbg.h 
# being a "GNU only extension", this is not true - it's part of C99 & 
# C11, but I have no idea why Clang is complaining about it. 
########################################################################
OTPROOT?=/usr/local/homebrew/Cellar/erlang/R15B03-1/lib/erlang/
EI=lib/erl_interface-3.7.9/
EI_H=${EI}include
EI_LIB=${EI}/lib
CFLAGS=-g -Wall -Werror -pedantic -Wno-gnu -O3 -fPIC -I${OTPROOT}${EI_H} -Ic_src/mutton/include
LDFLAGS= -L${OTPROOT}${EI_LIB} -lei_st c_src/mutton/build/lib/libmutton.dylib 
# this is used by the build_deps.sh script for compiling libmutton
export ERLANG_ARCH=64

exe=emtn_prog

all: $(exe)

deps:
	bash ./c_src/build_deps.sh
# c program can't `mkdir -p {path}`, so just force a /tmp directory for it
	bash ./c_src/build_deps.sh lua
	bash ./setup.sh

emtn_prog: deps 
	cc $(CFLAGS) ./c_src/*.c -o ./priv/$(exe) $(LDFLAGS)
	erlc -o ./ebin ./src/*.erl

clean:
	rm -f priv/*.so
	rm -f priv/$(exe)
	rm -rf priv/$(exe).dSYM
	rm -f ebin/*.beam
	rm -f c_src/*.o
	rm -f $(exe)
	rm -rf $(exe).dSYM
	rm -rf tmp
	bash ./c_src/build_deps.sh clean 
