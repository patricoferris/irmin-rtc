open Lwt.Infix

module Make (S : Irmin.S) = struct
  module X = struct
    include S.Backend

    module Remote = struct
      type t = S.t
      type endpoint = Rtc.DataChannel.t
      type commit = S.Backend.Remote.commit
      type branch = S.branch
      type bundle = S.Backend.Slice.t * S.node_key

      let bundle_t = Irmin.Type.pair S.Backend.Slice.t S.node_key_t

      type incoming = Fetch of S.branch | Bundle of bundle [@@deriving irmin]

      let v = S.main

      let fetch t ?depth:_ dc branch : (commit option, _) result Lwt.t =
        let complete, set_complete = Lwt.wait () in
        Brr.Console.log [ Jstr.v "Fetching..." ];
        Rtc.DataChannel.Ev.set_on_message
          (fun msg ->
            let s : Jstr.t = Brr_io.Message.Ev.data (Brr.Ev.as_type msg) in
            match Irmin.Type.of_json_string bundle_t (Jstr.to_string s) with
            | Ok (v, c) -> Lwt.wakeup_later set_complete (Ok (v, c))
            | Error _ as e -> Lwt.wakeup_later set_complete e)
          dc;
        Rtc.DataChannel.send
          (Jstr.v
          @@ Irmin.Type.to_json_string ~minify:true incoming_t (Fetch branch))
          dc;
        complete >>= fun res ->
        (match res with
        | Error _ as e -> Lwt.return e
        | Ok (slice, h) -> (
            let repo = S.repo t in
            (* Check for commit in store *)
            S.Commit.of_hash repo h >>= function
            | Some c ->
              S.Head.set t c >>= fun () ->
              Lwt.return_ok (Some h)
            | None -> (
                S.Repo.import repo slice >>= fun v ->
                match v with
                | Error _ as e -> Lwt.return e
                | Ok () -> (
                    S.Commit.of_hash repo h >>= function
                    | Some c ->
                      S.Head.set t c >>= fun () ->
                      Lwt.return_ok (Some h)
                    | None -> Lwt.return (Error (`Msg "Failed to import slice"))
                    ))))


      let push t ?depth dc (branch : branch) =
        let repo = S.repo t in
        S.Branch.get repo branch >>= fun branch_commit ->
        S.Repo.export ?depth repo ~max:(`Max [ branch_commit ]) >>= fun slice ->
        let ser =
          Irmin.Type.to_json_string ~minify:true bundle_t
            (slice, S.Commit.hash branch_commit)
        in
        Rtc.DataChannel.send (Jstr.v ser) dc;
        Lwt.return_ok ()
    end
  end

  include Irmin.Of_backend (X)

  let push t ?depth dc (branch : branch) =
    let repo = repo t in
    S.Branch.get repo branch >>= fun branch_commit ->
    S.Repo.export ?depth repo ~max:(`Max [ branch_commit ]) >>= fun slice ->
    let ser =
      Irmin.Type.to_json_string ~minify:true X.Remote.bundle_t
        (slice, S.Commit.hash branch_commit)
    in
    Rtc.DataChannel.send (Jstr.v ser) dc;
    Lwt.return_ok @@ Rtc.DataChannel.close dc

  let rtc_serve (s : t) (r : Rtc.DataChannel.t) =
    Rtc.DataChannel.Ev.set_on_message
      (fun msg ->
        let st : Jstr.t = Brr_io.Message.Ev.data (Brr.Ev.as_type msg) in
        match
          Irmin.Type.of_json_string X.Remote.incoming_t (Jstr.to_string st)
        with
        | Ok (Fetch br) ->
            Brr.Console.log [ Jstr.v "Pushing: "; br ];
            push s r br
        | Ok _ -> Lwt.return (Ok ())
        | Error _ -> failwith "TODO")
      r
end
