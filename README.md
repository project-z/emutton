# `emutton` - Erlang bindings to [Mutton](https://github.com/project-z/mutton) core bitmap indexing library

## Erlang-to-C interoperation

Currently, `emutton` uses "[plain port](http://www.erlang.org/doc/tutorial/c_port.html)" interoperation with Erlang.  This means that Erlang communicates with the C wrapper of libmutton via standard input and standard output.  The benefit of this is that the wrapper is running outside of the Erlang VM. If it crashes, it will not take down the Erlang VM with it.  A supervisor can restart the external program and remain communicating with it.  Erlang has 4 levels of support for interoperating with C programs (1. "plain ports", 2. port_drivers, 3. cnodes, & 4. native interface functions).  Though "plain ports" are the slowest, they are the safest and will allow the transition to a faster form of communication.  As the C wrapper of libmutton becomes more efficient, stable moving to another (faster) approach can be done.

For more on Erlang interoperability, see the [user guide](http://www.erlang.org/doc/tutorial/users_guide.html).

## Examples

Working from the Erlang, here is the interaction with the ```emtn``` module:

```
$ erl -pa ../emutton/ebin/
Erlang R15B03 (erts-5.9.3.1) [source] [64-bit] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:false] [dtrace]

Eshell V5.9.3.1  (abort with ^G)
1> emtn:start().
ok
2> emtn:ping().
pong
3> emtn:status(9).
{ok,9}
4> emtn:cwd().
{cwd,"../emutton/priv"}
5> BucketName = "Hoth-Coffee-Base".
"Hoth-Coffee-Base"
6> EventName = "basic".
"basic"
7> EventPayload = "{\"a_field\":\"sign-up; campaign=20130701\"}".
"{\"a_field\":\"sign-up; campaign=20130701\"}"
8> emtn:index(BucketName, EventName, EventPayload).
                                  {ok,["Hoth-Coffee-Base","basic",
     "{\"a_field\":\"sign-up; campaign=20130701\"}"]}
9> emtn:stop().
650
10>
```

## Building emutton

`emutton` uses [rebar](https://github.com/basho/rebar) for building and running unit tests.

### What `emutton` Depends On

* Erlang
* Mutton
** leveldb
** lua
** Cassandra

You'll need a version of Erlang/OTP R15B are newer.  If you're working on Mac OS X, you can just install with Homebrew:

```
brew install erlang
```

After you clone this repo, you can run the following commands:

```
rebar compile
```

The end of a successful compile will look something like this:

```
*** No errors detected
bash ./build_deps.sh lua
moving lua script over to the top level directory....
bash ./setup.sh
cc -g -Wall -Werror -pedantic -Wno-gnu -O3 -fPIC -I/usr/local/homebrew/Cellar/erlang/R15B03-1/lib/erlang/lib/erl_interface-3.7.9/include -Imutton/include ./*.c -o ../priv/emtn_prog -L/usr/local/homebrew/Cellar/erlang/R15B03-1/lib/erlang/lib/erl_interface-3.7.9//lib -lerl_interface -lei mutton/build/lib/libmutton.dylib
Compiled src/emtn.erl
Compiling c_src/emtn_prog.c
```

This will compile both the Erlang and C code.  It will also pull the latest version of Mutton from master (note: it will pull stable, tagged releases as those are made available).

```
rebar eunit
```

If all has gone well, it compiled & you'll see that all tests have passed:

```
==> emutton (eunit)
  All 5 tests passed.
```

(TODO: another dependency is that you'll need a Cassandra cluster - or we'll need a way to set an option to only index... emutton is calling Mutton and it will index your data and store it the raw content in Cassandra using libcql)
