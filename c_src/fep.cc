#include "fep.h"
#include "fep_nif.h"
#include "nif_utils.h"
#include "concurrentqueue.h"

struct squeue {
    moodycamel::ConcurrentQueue<ErlNifPid> *queue;
};

void nif_enlfq_free(ErlNifEnv *, void *obj) {
    squeue *inst = static_cast<squeue *>(obj);

    if (inst != nullptr)
        delete inst->queue;
}

ERL_NIF_TERM nif_fep_new(ErlNifEnv *env, int, const ERL_NIF_TERM *) {
    shared_data *data = static_cast<shared_data *>(enif_priv_data(env));

    squeue *qinst = static_cast<squeue *>(enif_alloc_resource(data->resQueueInstance, sizeof(squeue)));
    qinst->queue = new moodycamel::ConcurrentQueue<ErlNifPid>;

    if (qinst == NULL)
        return make_error(env, "enif_alloc_resource failed");

    ERL_NIF_TERM term = enif_make_resource(env, qinst);
    enif_release_resource(qinst);
    return enif_make_tuple2(env, ATOMS.atomOk, term);
}

ERL_NIF_TERM nif_fep_push(ErlNifEnv *env, int, const ERL_NIF_TERM argv[]) {
    shared_data *data = static_cast<shared_data *>(enif_priv_data(env));

    squeue *inst = NULL;

    if (!enif_get_resource(env, argv[0], data->resQueueInstance, (void **) &inst)) {
        return enif_make_badarg(env);
    }

    ErlNifPid self;
    if (enif_self(env, &self) == NULL) {
        return enif_make_badarg(env);
    }

    inst->queue->enqueue(self);

    return ATOMS.atomTrue;
}

ERL_NIF_TERM nif_fep_pop(ErlNifEnv *env, int, const ERL_NIF_TERM argv[]) {
    shared_data *data = static_cast<shared_data *>(enif_priv_data(env));
    squeue *inst = NULL;

    if (!enif_get_resource(env, argv[0], data->resQueueInstance, (void **) &inst)) {
        return enif_make_badarg(env);
    }

    ErlNifPid pid;

    if (inst->queue->try_dequeue(pid)) {
        return enif_make_tuple2(env, ATOMS.atomOk, enif_make_pid(env, &pid));
    } else {
        return ATOMS.atomEmpty;
    }

}

ERL_NIF_TERM nif_fep_pop_if_alive(ErlNifEnv *env, int, const ERL_NIF_TERM argv[]) {
    shared_data *data = static_cast<shared_data *>(enif_priv_data(env));
    squeue *inst = NULL;

    if (!enif_get_resource(env, argv[0], data->resQueueInstance, (void **) &inst)) {
        return enif_make_badarg(env);
    }

    ErlNifPid pid;

    if (inst->queue->try_dequeue(pid)) {
        if (!enif_is_process_alive(env, &pid)) {
            return ATOMS.atomDead;
        }
        return enif_make_tuple2(env, ATOMS.atomOk, enif_make_pid(env, &pid));
    } else {
        return ATOMS.atomEmpty;
    }

}