#include <crm114/crm114.h>
#include "erl_nif.h"

/* Static Erlang Terms */

#define ATOM(Id, Value) { Id = enif_make_atom(env, Value); }

static ERL_NIF_TERM ATOM_EALLOC;
static ERL_NIF_TERM ATOM_ERROR;
static ERL_NIF_TERM ATOM_OK;

static ERL_NIF_TERM ATOM_CLASSES;
static ERL_NIF_TERM ATOM_REGEX;

static ERL_NIF_TERM ATOM_FAILURE;
static ERL_NIF_TERM ATOM_SUCCESS;


static ERL_NIF_TERM ATOM_CRM114_CROSSLINK;
static ERL_NIF_TERM ATOM_CRM114_ENTROPY;
static ERL_NIF_TERM ATOM_CRM114_FSCM;
static ERL_NIF_TERM ATOM_CRM114_HYPERSPACE;
static ERL_NIF_TERM ATOM_CRM114_MARKOVIAN;
static ERL_NIF_TERM ATOM_CRM114_MICROGROOM;
static ERL_NIF_TERM ATOM_CRM114_OSB;
static ERL_NIF_TERM ATOM_CRM114_OSBF;
static ERL_NIF_TERM ATOM_CRM114_PCA;
static ERL_NIF_TERM ATOM_CRM114_STRING;
static ERL_NIF_TERM ATOM_CRM114_SVM;
static ERL_NIF_TERM ATOM_CRM114_UNIGRAM;
static ERL_NIF_TERM ATOM_CRM114_UNIQUE;

static ERL_NIF_TERM LIST_EMPTY;

/* ErlCRM114 Classifier API */

static ERL_NIF_TERM ErlCRM114_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM ErlCRM114_learn(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM ErlCRM114_classify(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static void         ErlCRM114_free(ErlNifEnv *env, void *res);

typedef unsigned long long ErlCRM114Flag;
typedef struct {
  CRM114_CONTROLBLOCK *cb;
  CRM114_DATABLOCK *db;
} ErlCRM114Classifier;

static ErlNifResourceType *ErlCRM114ClassifierType;

/* Support Functions */

#define TERM_EQ(lhs, rhs) (enif_compare(lhs, rhs) == 0)

static ERL_NIF_TERM
make_reference(ErlNifEnv *env, void *res) {
  ERL_NIF_TERM ref = enif_make_resource(env, res);
  enif_release_resource(res);
  return ref;
}

/* ErlCRM114 Classifier Implementation */

static ErlCRM114Flag
ErlCRM114_new_flag(ErlNifEnv *env, const ERL_NIF_TERM atom) {
  if (TERM_EQ(atom, ATOM_CRM114_CROSSLINK))
    return CRM114_CROSSLINK;
  else if(TERM_EQ(atom, ATOM_CRM114_ENTROPY))
    return CRM114_ENTROPY;
  else if(TERM_EQ(atom, ATOM_CRM114_FSCM))
    return CRM114_FSCM;
  else if(TERM_EQ(atom, ATOM_CRM114_HYPERSPACE))
    return CRM114_HYPERSPACE;
  else if(TERM_EQ(atom, ATOM_CRM114_MARKOVIAN))
    return CRM114_MARKOVIAN;
  else if(TERM_EQ(atom, ATOM_CRM114_MICROGROOM))
    return CRM114_MICROGROOM;
  else if(TERM_EQ(atom, ATOM_CRM114_OSB))
    return CRM114_OSB;
  else if(TERM_EQ(atom, ATOM_CRM114_OSBF))
    return CRM114_OSBF;
  else if(TERM_EQ(atom, ATOM_CRM114_PCA))
    return CRM114_PCA;
  else if(TERM_EQ(atom, ATOM_CRM114_STRING))
    return CRM114_STRING;
  else if(TERM_EQ(atom, ATOM_CRM114_SVM))
    return CRM114_SVM;
  else if(TERM_EQ(atom, ATOM_CRM114_UNIGRAM))
    return CRM114_UNIGRAM;
  else if(TERM_EQ(atom, ATOM_CRM114_UNIQUE))
    return CRM114_UNIQUE;
  return -1;
}

static ERL_NIF_TERM
ErlCRM114_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  ErlCRM114Classifier *classifier = NULL;
  ErlCRM114Flag flags = 0;
  ErlNifBinary regex = {0};
  ERL_NIF_TERM head, classes = LIST_EMPTY, opts = argv[0];
  int i, arity;
  const ERL_NIF_TERM *tuple;
  CRM114_CONTROLBLOCK *cb = crm114_new_cb();
  CRM114_DATABLOCK *db = NULL;

  if (cb == NULL)
    goto ealloc;

  if (!enif_is_list(env, opts))
    goto badarg;
  while (enif_get_list_cell(env, opts, &head, &opts)) {
    if (enif_is_atom(env, head))
      flags |= ErlCRM114_new_flag(env, head);
    else if (enif_get_tuple(env, head, &arity, &tuple) && arity == 2)
      if (enif_compare(tuple[0], ATOM_CLASSES) == 0)
        classes = tuple[1];
      else if (enif_compare(tuple[0], ATOM_REGEX) == 0) {
        if (!enif_inspect_binary(env, tuple[1], &regex))
          goto badarg;
      } else
        goto badarg;
    else
      goto badarg;
  }

  if (crm114_cb_setflags(cb, flags) != CRM114_OK)
    goto badarg;
  crm114_cb_setclassdefaults(cb);

  // these casts should be safe since tre casts back to unsigned
  if (regex.size)
    if (crm114_cb_setregex(cb, (char *)regex.data, (int)regex.size) != CRM114_OK)
      goto badarg;

  if (!TERM_EQ(classes, LIST_EMPTY)) {
    // this cast is potentially unsafe for a large number of classes
    if (!enif_get_list_length(env, classes, (unsigned *)&cb->how_many_classes))
      goto badarg;
    for (i = 0; enif_get_list_cell(env, classes, &head, &classes); i++) {
      ERL_NIF_TERM name = head;
      if (enif_get_tuple(env, head, &arity, &tuple)) {
        name = tuple[0];
        if (TERM_EQ(tuple[1], ATOM_SUCCESS))
          cb->class[i].success = 1;
        else if (TERM_EQ(tuple[1], ATOM_FAILURE))
          cb->class[i].success = 0;
        else
          goto badarg;
      }
      if (!enif_get_string(env, name, cb->class[i].name, CLASSNAME_LENGTH, ERL_NIF_LATIN1))
        goto badarg;
    }
  }
  crm114_cb_setblockdefaults(cb);

  // create the data block
  db = crm114_new_db(cb);
  if (db == NULL)
    goto ealloc;

  // create the resource
  classifier = enif_alloc_resource(ErlCRM114ClassifierType, sizeof(ErlCRM114Classifier));
  if (classifier == NULL)
    goto ealloc;
  classifier->cb = cb;
  classifier->db = db;
  return enif_make_tuple2(env, ATOM_OK, make_reference(env, classifier));

 badarg:
  if (db) free(db);
  if (cb) free(cb);
  return enif_make_badarg(env);

 ealloc:
  if (db) free(db);
  if (cb) free(cb);
  return enif_make_tuple(env, ATOM_ERROR, ATOM_EALLOC);
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
ErlCRM114_free(ErlNifEnv *env, void *res) {
  ErlCRM114Classifier *classifier = (ErlCRM114Classifier *)res;
  free(classifier->db);
  free(classifier->cb);
}

/* NIF Initialization */

static ErlNifFunc nif_funcs[] =
  {
    {"new", 1, ErlCRM114_new},
    {"learn", 2, ErlCRM114_learn},
    {"classify", 2, ErlCRM114_classify},
  };

static int
on_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
  ErlNifResourceFlags flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
  ErlCRM114ClassifierType =
    enif_open_resource_type(env, NULL, "ErlCRM114Classifier", &ErlCRM114_free, flags, NULL);
  if (ErlCRM114ClassifierType == NULL)
    return -1;

  ATOM(ATOM_EALLOC, "ealloc");
  ATOM(ATOM_ERROR, "error");
  ATOM(ATOM_OK, "ok");
  ATOM(ATOM_CLASSES, "classes");
  ATOM(ATOM_REGEX, "regex");

  ATOM(ATOM_CRM114_CROSSLINK, "crosslink");
  ATOM(ATOM_CRM114_ENTROPY, "entropy");
  ATOM(ATOM_CRM114_FSCM, "fscm");
  ATOM(ATOM_CRM114_HYPERSPACE, "hyperspace");
  ATOM(ATOM_CRM114_MARKOVIAN, "markovian");
  ATOM(ATOM_CRM114_MICROGROOM, "microgroom");
  ATOM(ATOM_CRM114_OSB, "osb");
  ATOM(ATOM_CRM114_OSBF, "osbf");
  ATOM(ATOM_CRM114_PCA, "pca");
  ATOM(ATOM_CRM114_STRING, "string");
  ATOM(ATOM_CRM114_SVM, "svm");
  ATOM(ATOM_CRM114_UNIGRAM, "unigram");
  ATOM(ATOM_CRM114_UNIQUE, "unique");

  LIST_EMPTY = enif_make_list(env, 0);

  return 0;
}

ERL_NIF_INIT(erlcrm114, nif_funcs, &on_load, NULL, NULL, NULL);
