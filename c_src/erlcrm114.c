#include <crm114/crm114.h>
#include <string.h>
#include "erl_nif.h"

/* Static Erlang Terms */

#define ATOM(Id, Value) { Id = enif_make_atom(env, Value); }

static ERL_NIF_TERM ATOM_BADARG;
static ERL_NIF_TERM ATOM_EALLOC;
static ERL_NIF_TERM ATOM_ERROR;
static ERL_NIF_TERM ATOM_OK;

static ERL_NIF_TERM ATOM_CLASSES;
static ERL_NIF_TERM ATOM_DETAIL;
static ERL_NIF_TERM ATOM_REGEX;
static ERL_NIF_TERM ATOM_RESULT;

static ERL_NIF_TERM ATOM_FAILURE;
static ERL_NIF_TERM ATOM_SUCCESS;

static ERL_NIF_TERM ATOM_CRM114_CROSSLINK;
static ERL_NIF_TERM ATOM_CRM114_ENTROPY;
static ERL_NIF_TERM ATOM_CRM114_HYPERSPACE;
static ERL_NIF_TERM ATOM_CRM114_MICROGROOM;
static ERL_NIF_TERM ATOM_CRM114_OSB;
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
static ERL_NIF_TERM ErlCRM114_from_binary(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM ErlCRM114_to_binary(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static void         ErlCRM114_free(ErlNifEnv *env, void *res);

typedef unsigned long long ErlCRM114Flag;
typedef struct {
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

static ERL_NIF_TERM
make_strerror(ErlNifEnv *env, const char *reason) {
  return enif_make_tuple2(env, ATOM_ERROR, enif_make_string(env, reason, ERL_NIF_LATIN1));
}

/* ErlCRM114 Classifier Implementation */

static ErlCRM114Flag
ErlCRM114_new_flag(ErlNifEnv *env, const ERL_NIF_TERM atom) {
  if (TERM_EQ(atom, ATOM_CRM114_CROSSLINK))
    return CRM114_CROSSLINK;
  else if(TERM_EQ(atom, ATOM_CRM114_ENTROPY))
    return CRM114_ENTROPY;
  else if(TERM_EQ(atom, ATOM_CRM114_HYPERSPACE))
    return CRM114_HYPERSPACE;
  else if(TERM_EQ(atom, ATOM_CRM114_MICROGROOM))
    return CRM114_MICROGROOM;
  else if(TERM_EQ(atom, ATOM_CRM114_OSB))
    return CRM114_OSB;
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
  ErlCRM114Classifier *classifier;
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
      if (enif_get_tuple(env, head, &arity, &tuple) && arity == 2) {
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
  free(cb);

  // create the resource
  classifier = enif_alloc_resource(ErlCRM114ClassifierType, sizeof(ErlCRM114Classifier));
  if (classifier == NULL)
    goto ealloc;
  classifier->db = db;
  return enif_make_tuple2(env, ATOM_OK, make_reference(env, classifier));

 badarg:
  if (db) free(db);
  if (cb) free(cb);
  return enif_make_tuple2(env, ATOM_ERROR, ATOM_BADARG);

 ealloc:
  if (db) free(db);
  if (cb) free(cb);
  return enif_make_tuple2(env, ATOM_ERROR, ATOM_EALLOC);
}

static ERL_NIF_TERM
ErlCRM114_learn(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  ErlCRM114Classifier *classifier;
  ErlNifBinary text;
  CRM114_ERR status;
  int classnum = 0;

  if (!enif_get_resource(env, argv[0], ErlCRM114ClassifierType, (void **)&classifier))
    goto badarg;
  if (!enif_inspect_iolist_as_binary(env, argv[1], &text))
    goto badarg;
  if (argc > 2)
    if (!enif_get_int(env, argv[2], &classnum))
      goto badarg;

  status = crm114_learn_text(&classifier->db, classnum, (char *)text.data, text.size);
  if (status == CRM114_BADARG)
    goto badarg;
  if (status != CRM114_OK)
    goto crmerr;
  return enif_make_tuple2(env, ATOM_OK, argv[0]);

 badarg:
  return enif_make_tuple2(env, ATOM_ERROR, ATOM_BADARG);

 crmerr:
  return make_strerror(env, crm114_strerror(status));
}

static ERL_NIF_TERM
ErlCRM114_classify(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  ErlCRM114Classifier *classifier;
  ErlNifBinary text;
  ERL_NIF_TERM head, opts, classes;
  CRM114_ERR status;
  CRM114_MATCHRESULT result;
  int i, detail = 0;

  if (!enif_get_resource(env, argv[0], ErlCRM114ClassifierType, (void **)&classifier))
    goto badarg;
  if (!enif_inspect_iolist_as_binary(env, argv[1], &text))
    goto badarg;
  if (argc > 2) {
    if (enif_is_list(env, opts = argv[2]))
      while (enif_get_list_cell(env, opts, &head, &opts))
        if (TERM_EQ(head, ATOM_DETAIL))
          detail = 1;
        else
          goto badarg;
    else
      goto badarg;
  }

  status = crm114_classify_text(classifier->db, (char *)text.data, text.size, &result);
  if (status == CRM114_BADARG)
    goto badarg;
  if (status != CRM114_OK)
    goto crmerr;

  if (detail) {
    classes = LIST_EMPTY;
    for (i = 0; i < result.how_many_classes; i++)
      classes =
        enif_make_list_cell(env,
                            enif_make_tuple8(env,
                                             ATOM_DETAIL,
                                             enif_make_double(env, result.class[i].pR),
                                             enif_make_double(env, result.class[i].prob),
                                             enif_make_string(env, result.class[i].name, ERL_NIF_LATIN1),
                                             enif_make_int(env, result.class[i].hits),
                                             enif_make_int(env, result.class[i].features),
                                             enif_make_int(env, result.class[i].documents),
                                             result.class[i].success ? ATOM_SUCCESS : ATOM_FAILURE),
                            classes);
  } else {
    classes = enif_make_int(env, result.how_many_classes);
  }

  return enif_make_tuple6(env,
                          ATOM_RESULT,
                          enif_make_double(env, result.overall_pR),
                          enif_make_double(env, result.tsprob),
                          enif_make_int(env, result.bestmatch_index),
                          enif_make_int(env, result.unk_features),
                          classes);

 badarg:
  return enif_make_tuple2(env, ATOM_ERROR, ATOM_BADARG);

 crmerr:
  return make_strerror(env, crm114_strerror(status));
}

static ERL_NIF_TERM
ErlCRM114_from_binary(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  ErlCRM114Classifier *classifier;
  ErlNifBinary binary;
  CRM114_DATABLOCK *db = NULL;

  if (!enif_inspect_binary(env, argv[0], &binary))
    goto badarg;

  db = malloc(binary.size);
  if (db == NULL)
    goto ealloc;
  memcpy(db, binary.data, binary.size);

  classifier = enif_alloc_resource(ErlCRM114ClassifierType, sizeof(ErlCRM114Classifier));
  if (classifier == NULL)
    goto ealloc;
  classifier->db = db;
  return enif_make_tuple2(env, ATOM_OK, make_reference(env, classifier));

 badarg:
  if (db) free(db);
  return enif_make_tuple2(env, ATOM_ERROR, ATOM_BADARG);

 ealloc:
  if (db) free(db);
  return enif_make_tuple2(env, ATOM_ERROR, ATOM_EALLOC);
}

static ERL_NIF_TERM
ErlCRM114_to_binary(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  ErlCRM114Classifier *classifier;
  ErlNifBinary binary;

  if (!enif_get_resource(env, argv[0], ErlCRM114ClassifierType, (void **)&classifier))
    goto badarg;
  if (!enif_alloc_binary(classifier->db->cb.datablock_size, &binary))
    goto ealloc;
  memcpy(binary.data, classifier->db, binary.size);
  return enif_make_binary(env, &binary);

 badarg:
  return enif_make_tuple2(env, ATOM_ERROR, ATOM_BADARG);

 ealloc:
  return enif_make_tuple2(env, ATOM_ERROR, ATOM_EALLOC);
}

static void
ErlCRM114_free(ErlNifEnv *env, void *res) {
  ErlCRM114Classifier *classifier = (ErlCRM114Classifier *)res;
  free(classifier->db);
}

/* NIF Initialization */

static ErlNifFunc nif_funcs[] =
  {
    {"new", 1, ErlCRM114_new},
    {"learn", 2, ErlCRM114_learn},
    {"learn", 3, ErlCRM114_learn},
    {"classify", 2, ErlCRM114_classify},
    {"classify", 3, ErlCRM114_classify},
    {"from_binary", 1, ErlCRM114_from_binary},
    {"to_binary", 1, ErlCRM114_to_binary},
  };

static int
on_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
  ErlNifResourceFlags flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
  ErlCRM114ClassifierType =
    enif_open_resource_type(env, NULL, "ErlCRM114Classifier", &ErlCRM114_free, flags, NULL);
  if (ErlCRM114ClassifierType == NULL)
    return -1;

  ATOM(ATOM_BADARG, "badarg");
  ATOM(ATOM_EALLOC, "ealloc");
  ATOM(ATOM_ERROR, "error");
  ATOM(ATOM_OK, "ok");

  ATOM(ATOM_CLASSES, "classes");
  ATOM(ATOM_DETAIL, "detail");
  ATOM(ATOM_REGEX, "regex");
  ATOM(ATOM_RESULT, "result");

  ATOM(ATOM_FAILURE, "failure");
  ATOM(ATOM_SUCCESS, "success");

  ATOM(ATOM_CRM114_CROSSLINK, "crosslink");
  ATOM(ATOM_CRM114_ENTROPY, "entropy");
  ATOM(ATOM_CRM114_HYPERSPACE, "hyperspace");
  ATOM(ATOM_CRM114_MICROGROOM, "microgroom");
  ATOM(ATOM_CRM114_OSB, "osb");
  ATOM(ATOM_CRM114_PCA, "pca");
  ATOM(ATOM_CRM114_STRING, "string");
  ATOM(ATOM_CRM114_SVM, "svm");
  ATOM(ATOM_CRM114_UNIGRAM, "unigram");
  ATOM(ATOM_CRM114_UNIQUE, "unique");

  LIST_EMPTY = enif_make_list(env, 0);

  return 0;
}

ERL_NIF_INIT(erlcrm114, nif_funcs, &on_load, NULL, NULL, NULL);
