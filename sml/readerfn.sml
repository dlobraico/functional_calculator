(* readerfn.sml *)
(* Purely Functional Input, functor version *)

signature READER =
sig
  type instream
  type 'a reader = instream -> ('a * instream) option
  val charReader : char reader
  val return : 'a -> 'a reader
  val fail : 'a reader
  val chain: 'a reader -> ('a -> 'b reader) -> 'b reader
  val choice: 'a reader * 'a reader -> 'a reader
  val star : 'a reader -> 'a list reader 
  val starPlus : 'a reader -> 'a list reader
end

functor ReaderFn(X : INSTREAM) : READER where type instream = X.instream =
struct

  open X

  (* an 'a reader is a function that consumes some prefix
   * of its stream argument and produces an 'a and 
   * the remainder of the stream if successful. As a
   * function, a reader action is performed by applying it
   * to an instream. *)
  type 'a reader = instream -> ('a * instream) option

  val charReader = X.inputc

  (* "monadic operations for readers: return, chain, fail *)

  (* return :  'a -> 'a reader *)
  fun return v = fn instream => SOME(v, instream)

  (* failure : 'a reader *)
  val fail : 'a reader = fn instream => NONE

  (* chain: 'a reader -> ('a -> 'b reader) -> 'b reader
   * this chains together a reader, and another reader computed
   * from the value produced by the first reader (if successful). *)
  fun chain (p: 'a reader) (f: 'a -> 'b reader) instream =
      case p instream
	of NONE => NONE
	 | SOME(v, instream') => (f v) instream'

  (* choice: 'a reader * 'a reader -> 'a reader  *)
  (* perform p, but if it fails, perform q on same instream *)
  fun choice (p, q) instream =
      case p instream
	of NONE => q instream
	 | res => res

  (* star : 'a reader -> 'a list reader 
   * A "reader transform" that corresponds to the Kleene star. *)
  fun star r = choice (starPlus r, return [])

  (* starPlus : 'a reader -> 'a list reader *)
  and starPlus r = chain r (fn v => chain (star r) (fn vs => return (v::vs)))

end (* functor ReaderFn *)
