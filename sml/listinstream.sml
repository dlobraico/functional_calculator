(* listinstream.sml *)

structure ListInstream :> INSTREAM =
struct
   type instream = char list

   fun fromString string = explode string

   fun inputc [] = NONE
     | inputc (c::cs) = SOME(c, cs)
end
