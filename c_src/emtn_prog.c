#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>  // for stat() & mkdir()
#include <unistd.h>
#include <stdbool.h>
#include <string.h>
#include <dirent.h>

#include "erl_interface.h"
#include "ei.h"
#include "dbg.h"
#include "libmutton/mutton.h"

#define MAX_PATH_LEN 4096
#define MAX_EVENT_SCRIPTS 120
#define MAX_BUFFER 4096

const static char* DB_PATH_TOP = "/mtn";
const static char* DB_PATH_IDX = "/idx_store";
const static mtn_index_partition_t INDEX_PARTITION = 1;
const static char* BUCKET_NAME = "planet-hoth";

const static char* BASIC_EVENT_NAME = "basic";
const static char* BASIC_EVENT_JSON = "{\"a_field\":\"TROLOLOL I'm an Event!!!\"}";

const static char* LUA_SCRIPT_EXT = ".lua";
const static char* LUA_SCRIPT_PATH = "/lua_scripts/";

const static char* LUA_PACKAGE_PATH = "/lua_scripts/packages/?.lua";
const static char* LUA_LIB_PATH = "/lua_scripts/lib/?";

typedef unsigned char byte;

// rock the forward declarations... (based on Erlang plain ports example)
int read_cmd(byte *bufr);
int read_exact(byte *bufr, int len);
int write_cmd(byte *bufr, int len);
int write_exact(byte *bufr, int len);

int read_cmd(byte *bufr)
{
    int len;
    // the 1st byte will tell use how much data we'll have...
    if(read_exact(bufr, 2) != 2) {
        return -1;
    }
    len = (bufr[0] << 8) | bufr[1];
    return read_exact(bufr, len);
}

int write_cmd(byte *bufr, int len)
{
    byte li;

    li = (len >> 8) & 0xff;
    write_exact(&li, 1);

    li = len & 0xff;
    write_exact(&li, 1);

    return write_exact(bufr, len);
}

int read_exact(byte *bufr, int len)
{
    int i, got = 0;

    do {
        i = read(0, bufr + got, len - got);
        if(i <= 0) {
            return i;
        }
        got += i;
    } while(got < len);

    return len;
}

int write_exact(byte *bufr, int len)
{
    int i, wrote = 0;
    do {
        i = write(1, bufr + wrote, len - wrote);
        if(i <= 0) {
            return i;
        }
        wrote += i;
    } while(wrote < len);

    return len;
}

char** find_scripts(char *path, const char *extension) {
    char **scripts;
    int i = 0;
    DIR *dir;
    struct dirent *ent;
    int malsize = -1;
    int path_len = strlen(path);

    scripts = malloc(MAX_EVENT_SCRIPTS * sizeof(char *));
    scripts[MAX_EVENT_SCRIPTS-1] = NULL;

    dir = opendir(path);
    if(dir != NULL) {
        while((ent = readdir(dir)) != NULL) {
            if(strstr(ent->d_name, extension)) {
                malsize = (path_len + strlen(ent->d_name) + 1) * sizeof(char);
                scripts[i] = malloc(malsize);

                strlcpy(scripts[i], path, malsize);
                strlcat(scripts[i], ent->d_name, malsize);
                printf("found one: %s\n", scripts[i]);
                i++;
            }
        }
        closedir(dir);
    } else {
        // FAIL!
        return NULL;
    }

    scripts[i] = NULL;

    return scripts;
}

void *initialize_mutton(const char *curr_working_dir)
{
    struct stat st;
    void *status = NULL;
    bool ret = false;
    int rc;
    char package_path[MAX_PATH_LEN];
    char library_path[MAX_PATH_LEN];
    char script_path[MAX_PATH_LEN];
    char db_path[MAX_PATH_LEN];
    char **scripts = NULL;
    char *emessage = NULL;
    char **curr_script = NULL;
    void *ctxt = mutton_new_context();

    check(ctxt, "Well, that wasn't what we were expecting.");

    // set up all the paths to be relative to the provided working directory
    strlcpy(package_path, curr_working_dir, MAX_PATH_LEN);
    strlcat(package_path, LUA_PACKAGE_PATH, MAX_PATH_LEN);

    strlcpy(library_path, curr_working_dir, MAX_PATH_LEN);
    strlcat(library_path, LUA_LIB_PATH, MAX_PATH_LEN);

    strlcpy(script_path, curr_working_dir, MAX_PATH_LEN);
    strlcat(script_path, LUA_SCRIPT_PATH, MAX_PATH_LEN);

    strlcpy(db_path, curr_working_dir, MAX_PATH_LEN);
    strlcat(db_path, DB_PATH_TOP, MAX_PATH_LEN);

    if(stat(db_path, &st) == -1) {
        // so mkdir() doesn't expose a `-p` option to make any element along a
        // path that doesn't already exist... FutureMe - forgive PastYou.
        rc = mkdir(db_path, 0770);
        check(rc == 0, "We didn't make the directory (%s) after all... %d cwd: %s",
            db_path, rc, curr_working_dir);
        // tack on the final directory & then create that...
        strlcat(db_path, DB_PATH_IDX, MAX_PATH_LEN);
        rc = mkdir(db_path, 0770);
        check(rc == 0, "We didn't make the directory (%s) after all... %d cwd: %s",
            db_path, rc, curr_working_dir);
    } else { // this is ugly... FIX IT
        // if the path exists, we still have a "partial" path in ``db_path``,
        // so we've got to concatenate the final path element, "idx_store":
        strlcat(db_path, DB_PATH_IDX, MAX_PATH_LEN);
    }

    ret = mutton_set_opt(ctxt, MTN_OPT_DB_PATH, (void *)db_path, strlen(db_path), &status);
    check(ret, "Could not set option for the database path...");

    ret = mutton_set_opt(ctxt, MTN_OPT_LUA_PATH, (void *)package_path,
                        strlen(package_path), &status);
    check(ret, "Could not set option for lua package path...");


    ret = mutton_set_opt(ctxt, MTN_OPT_LUA_CPATH, (void *)library_path,
                        strlen(library_path), &status);
    check(ret, "Could not set option for lua library path...");

    ret = mutton_init_context(ctxt, &status);
    check(ret, "Could not initialize the context for Mutton...");

    if (ret) {
        printf("well - you don't have to fall on your sword yet...\n");
    }

    scripts = find_scripts(script_path, LUA_SCRIPT_EXT);
    check(scripts, "Something went horribly wrong with finding the scripts...");

    for(curr_script = scripts; *curr_script; curr_script++) {
        char *basename = NULL;
        char *p = NULL;
        int basename_len = 0;

        basename = strrchr(*curr_script, '/') + sizeof(char);
        basename_len = strlen(basename) - strlen(LUA_SCRIPT_EXT);
        p = malloc((basename_len + 1) * sizeof(char));
        strncpy(p, basename, basename_len);
        p[basename_len] = '\0';
        basename = p;

        ret = mutton_register_script_path(ctxt, MTN_SCRIPT_LUA, (void *)basename,
                    strlen(basename), (void *)(*curr_script), strlen(*curr_script),
                    &status);
        check(ret, "Could not register script paths correctly...");
        free(basename);
    }

    free(scripts);

    return ctxt;

error:

    mutton_status_get_message(ctxt, status, &emessage);
    log_err("mutton status msg: %s\n", emessage);
    free(emessage);

    // let's clean up before we exit:
    if(scripts) free(scripts);
    mutton_free_context(ctxt);
    mutton_free_status(status);

    return NULL;
}


int main(int argc, char *argv[])
{
    void *status = NULL;
    char *emessage = NULL;
    bool ret = false;
    ETERM *tuplep;
    ETERM *fnp, *argp, *tp;
    ETERM *result;
    char *bucket_name, *event_name, *event_json;
    byte bufr[MAX_BUFFER];
    char *curr_working_dir = NULL;
    void *ctxt = NULL;

    if(argc == 2) {
        // okay, so argv[0] is emtn_prog; we expect CWD in argv[1]
        curr_working_dir = argv[1];
    }

    ctxt = initialize_mutton(curr_working_dir);
    check(ctxt, "Well, that wasn't what we were expecting.");

    // only call this once - the Erl_Interface must be initalized before other
    // functions from erl_interface.h can be called.
    erl_init(NULL, 0);

    for(; read_cmd(bufr) > 0; ) {
        tuplep = erl_decode(bufr);

        fnp = erl_element(1, tuplep);

        if (strncmp(ERL_ATOM_PTR(fnp), "done", 4) == 0) {
            result = erl_mk_int(650);
        } else if (strncmp(ERL_ATOM_PTR(fnp), "index", 5) == 0) {
            argp = erl_element(2, tuplep);
            // what argp contains is an Erlang tuple in the format:
            //
            //      {{bucket, BucketName},
            //       {event, EventName},
            //       {payload, EventPayload}}}
            //
            // get the bucket tagged tuple
            tp = erl_element(1, argp);
            tp = erl_element(2, tp);
            bucket_name = erl_iolist_to_string(tp);
            // get the event tagged tuple
            tp = erl_element(2, argp);
            tp = erl_element(2, tp);
            event_name = erl_iolist_to_string(tp);
            // get the payload tagged tuple
            tp = erl_element(3, argp);
            tp = erl_element(2, tp);
            event_json = erl_iolist_to_string(tp);
            ret = mutton_process_event_bucketed(ctxt, INDEX_PARTITION,
                        (void *)bucket_name, strlen(bucket_name),
                        (void *)event_name, strlen(event_name),
                        (void *)event_json, strlen(event_json),
                        &status);
            if(ret) {
                result = erl_format("{ok, [~s,~s,~s]}",
                                    bucket_name, event_name, event_json);
            } else {
                mutton_status_get_message(ctxt, status, &emessage);
                result = erl_format("{error, ~s, ~s}", emessage, event_json);
            }
        } else if (strncmp(ERL_ATOM_PTR(fnp), "status", 6) == 0) {
            argp = erl_element(2, tuplep);
            result = erl_format("{ok, ~i}", ERL_INT_VALUE(argp));
        } else if (strncmp(ERL_ATOM_PTR(fnp), "ping", 4) == 0) {
            result = erl_mk_atom("pong");
        } else if (strncmp(ERL_ATOM_PTR(fnp), "cwd", 3) == 0) {
            if (!curr_working_dir) {
                curr_working_dir = getcwd(NULL, MAX_PATH_LEN);
            }
            result = erl_format("{cwd, ~s}", curr_working_dir);
        } else {
            result = erl_format("{error, unknown message: %s}", ERL_ATOM_PTR(fnp));
        }
        ret = true;

        erl_encode(result, bufr);
        write_cmd(bufr, erl_term_len(result));

        // TODO - evaluate if we need to free all ETERM *ptrs here,
        // I think we're leaking memory on every iteration...

        // if we were told we're done, exit the event-wait loop...
        if(strncmp(ERL_ATOM_PTR(fnp), "done", 4) == 0) {
            break;
        }
    }

    result = erl_format("{bah, ~s, ~s, ~s}",
                        BUCKET_NAME, BASIC_EVENT_NAME, BASIC_EVENT_JSON);

    erl_free_compound(tuplep);
    erl_free_term(fnp);
    erl_free_term(argp);
    erl_free_term(tp);
    erl_free_term(result);

    mutton_free_context(ctxt);

    return 0;

error: // if we have an error in check(), we'll jump to here...
    mutton_status_get_message(ctxt, status, &emessage);
    printf("error msg: %s\n", emessage);
    free(emessage);
    // let's clean up before we exit:
    if(ctxt) mutton_free_context(ctxt);
    if(status) mutton_free_status(status);
    return -1;
}
