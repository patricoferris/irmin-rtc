module Make (S : Irmin.S) : sig
  (** An Irmin store that can pull and push using RTC Datachannels. *)
  include
    Irmin.S
      with type hash = S.hash
       and module Schema = S.Schema
       and type Backend.Remote.endpoint = Rtc.DataChannel.t

  val rtc_serve : t -> Rtc.DataChannel.t -> unit
  (** Sets up a store *)
end
