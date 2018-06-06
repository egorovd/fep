#pragma once

#include "erl_nif.h"

extern "C" {
void nif_enlfq_free(ErlNifEnv *env, void *obj);
ERL_NIF_TERM nif_fep_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM nif_fep_push(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM nif_fep_pop(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM nif_fep_pop_if_alive(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
}