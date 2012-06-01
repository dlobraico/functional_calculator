(* env.sml *)
(* general environment implemented as an ordered alist (association list) *)

structure Env =
struct
  type key = string
  type 'a env = (key * 'a) list

  val compare = String.compare

  exception Unbound of string

  val empty : 'a env = []

  fun insert (k: key, v, []) = [(k,v)]
    | insert (k, v, env as (k',v')::env') =
      (case compare(k,k')
         of (LESS | EQUAL) => (k,v)::env
          | GREATER => (k',v')::(insert(k,v,env')))

  fun bind (k: key, v: 'a, env: 'a env) = 
      insert (k, v, env)

  fun lookup (k: key, nil: 'b env) = raise (Unbound k)
    | lookup (k, (k',v)::env) = 
      (case compare(k,k')
         of LESS => raise (Unbound k)
          | EQUAL => v 
          | GREATER => lookup(k,env))

end (* structure Env *)

(* Notes

This is not a particularly efficient implementation of finite maps
over strings, but it will do for our purposes.  These environments
are polymorphic -- the type of environment bindings will be 
defined in eval.sml.

*)
