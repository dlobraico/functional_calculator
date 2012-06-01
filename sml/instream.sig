(* instream.sig *)

signature INSTREAM =
sig
  type instream
  val fromString : string -> instream
  val inputc : instream -> (char * instream) option
end
