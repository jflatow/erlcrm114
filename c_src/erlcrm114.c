#include <crm114/crm114.h>
#include "erl_nif.h"

#define ATOM(Id, Value) { Id = enif_make_atom(env, Value); }
static ERL_NIF_TERM ATOM_EALLOC;
static ERL_NIF_TERM ATOM_ERROR;
static ERL_NIF_TERM ATOM_OK;

static ERL_NIF_TERM ErlCRM114_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM ErlCRM114_learn(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM ErlCRM114_classify(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static void         ErlCRM114_free(ErlNifEnv *env, void *arg);

typedef struct {
  // XXX
} ErlCRM114Cls;

static ErlNifResourceType *ErlCRM114CLS;
static ErlNifFunc nif_funcs[] =
  {
    {"new", 0, ErlCRM114_new},
    {"learn", 2, ErlCRM114_learn},
    {"classify", 2, ErlCRM114_classify},
  };

static ERL_NIF_TERM
ErlCRM114_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  ErlCRM114Cls *cls = enif_alloc_resource(ErlCRM114CLS, sizeof(ErlCRM114Cls));
  CRM114_CONTROLBLOCK *cb = crm114_new_cb();
  if (cb == NULL)
    return enif_make_tuple2(env, ATOM_ERROR, ATOM_EALLOC);
  ERL_NIF_TERM term = enif_make_resource(env, cls);
  enif_release_resource(cls);
  return enif_make_tuple2(env, ATOM_OK, term);
}


static ERL_NIF_TERM
ErlCRM114_learn(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM
ErlCRM114_classify(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  return enif_make_atom(env, "ok");
}

static void
ErlCRM114_free(ErlNifEnv *env, void *arg) {
  /* Delete any dynamically allocated memory stored in erlcrm114_handle */
  /* ErlCRM114Cls *handle = (ErlCRM114Cls *)arg; */
}

static int
on_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
  ErlNifResourceFlags flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
  ErlCRM114CLS = enif_open_resource_type(env, NULL, "ErlCRM114Cls", &ErlCRM114_free, flags, NULL);
  if (ErlCRM114CLS == NULL)
    return -1;

  ATOM(ATOM_EALLOC, "ealloc");
  ATOM(ATOM_ERROR, "error");
  ATOM(ATOM_OK, "ok");
  return 0;
}

ERL_NIF_INIT(erlcrm114, nif_funcs, &on_load, NULL, NULL, NULL);
