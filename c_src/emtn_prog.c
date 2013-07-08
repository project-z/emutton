#include <stdio.h>
#include <unistd.h>
#include <ctype.h>
#include <stdbool.h>
#include <string.h>

#include "ei.h"
#include "dbg.h"
#include "libmutton/mutton.h"

const static char* DB_PATH = "tmp/demo";
const static mtn_index_partition_t INDEX_PARTITION = 1;
const static char* BUCKET_NAME = "i am a bucket";

const static char* BASIC_EVENT_NAME = "basic";
const static char* BASIC_EVENT_JSON = "{\"a_field\":\"TROLOLOL I'm an Event!!!\"}";

const static char* LUA_SCRIPT_EXT = ".lua";
const static char* LUA_SCRIPT_PATH = "src/lua_scripts";

const static char* LUA_PACKAGE_PATH = "src/lua_scripts/packages/?.lua";
const static char* LUA_LIB_PATH = "src/lua_scripts/lib/?";

typedef struct {
    ei_x_buff x;
    void *mtn_context;
} state_t;

typedef char byte;

read_cmd(byte *bufr)
{
    int len;

    if(read_exact(bufr, 2) != 2) return -1;

    len = (bufr[0] << 8) | bufr[1];

    return read_exact(bufr, len);

}

read_exact(byte *bufr, int len)
{
    int i, got=0;

    do {
        i = read(0, bufr+got, len-got);
        if(i <= 0) return i;
        got += i;
    } while(got < len);

    return len;
}

int main()
{
    int fn, arg, result;
    byte bufr[100];

    while(read_cmd(bufr) > 0) {
        fn = bufr[0];
        arg = bufr[1];
        if(fn == 1) {
            printf("print something about function: %d", fn);
        } else if (fn == 2) {
            printf("print something about function: %d", fn);
        }
        bufr[0] = "return value... TROLOLOLOL";
        result = 0;
    }

}