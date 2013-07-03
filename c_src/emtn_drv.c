
#include <erl_driver.h>
#include <ei.h>
#include <ctype.h>
#include <stdbool.h>
#include <string.h>

#include "dbg.h"
#include "libmutton/mutton.h"

typedef struct {
    ErlDrvPort port;
    void *context;
} emtn_drv_t;

static ErlDrvData start(ErlDrvPort port, char *cmd)
{
    emtn_drv_t *emtn = (emtn_drv_t *)driver_alloc(sizeof(emtn_drv_t));

    void *context = mutton_new_context();
    check(context, "Failed to create a Mutton Context");
    emtn->context = context;

    log_info("do something with cmd: %s", cmd);

    emtn->port = port;

    return (ErlDrvData)emtn;
error:
    driver_failure_posix(port, -1);
    return NULL;
}

static void stop(ErlDrvData handle) {
    emtn_drv_t *driver_data = (emtn_drv_t *)handle;
    check(driver_data, "Failed to cast the handle...");
    // do something to stop it and free resources...

error:
    log_info("well... I didn't expect that...");
}

static void outputv(ErlDrvData handle, ErlIOVec *ev) {
    emtn_drv_t *emtn = (emtn_drv_t *)handle;

    ErlDrvPort port = emtn->port;
    check(port, "Failed to get the port out of the handle");
    log_info("well, the outputv got called...");
    log_info("do something with ev: %p", ev);

error:
    log_info("bummer...");
}

static ErlDrvEntry emutton_driver_entry = {
    NULL,                               /* init */
    start,                              /* startup */
    stop,                               /* shutdown */
    NULL,                               /* output */
    NULL,                               /* ready_input */
    NULL,                               /* ready_output */
    "emtn_drv",                         /* name of the driver */
    NULL,                               /* finish */
    NULL,                               /* handle */
    NULL,                               /* control */
    NULL,                               /* timeout */
    outputv,                            /* outputv */
    NULL,                               /* ready_async */
    NULL,                               /* flush */
    NULL,                               /* call */
    NULL,                               /* event */
    ERL_DRV_EXTENDED_MARKER,            /* ERL_DRV_EXTENDED_MARKER */
    ERL_DRV_EXTENDED_MAJOR_VERSION,     /* ERL_DRV_EXTENDED_MAJOR_VERSION */
    ERL_DRV_EXTENDED_MAJOR_VERSION,     /* ERL_DRV_EXTENDED_MINOR_VERSION */
    ERL_DRV_FLAG_USE_PORT_LOCKING,      /* ERL_DRV_FLAGs */
    NULL,                               /* void *handle2, reversed for VM use */
    NULL,                               /* F_PTR process_exit, called when a monitored process dies */
    NULL                                /* F_PTR stop_select, called to close an event object */
};

DRIVER_INIT(emtn_drv) {
    return &emutton_driver_entry;
}