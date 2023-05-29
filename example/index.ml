(* An example showing how to sync two Irmin repositories in two
   browsers in a peer-to-peer way using RTC Datachannels. *)
open Brr

let find_element s = Document.find_el_by_id G.document (Jstr.v s)
let get_value el = Jv.get (El.to_jv el) "value" |> Jv.to_jstr
let set_value el v = Jv.set (El.to_jv el) "value" (Jv.of_jstr v)

let set_innerhtml el s =
  El.to_jv el |> fun v -> Jv.set v "innerHTML" (Jv.of_string s)

module Contents = struct
  type t = string list

  let of_text t =
    Code_mirror.Text.to_jstr_array t
    |> Array.map Jstr.to_string |> Array.to_list

  let t = Irmin.Type.(list string)
  let merge = Irmin.Merge.(option @@ default t)
end

module Maker =
  Irmin.Maker (Irmin_indexeddb.Content_store) (Irmin_indexeddb.Branch_store)

module Persistent_store = Maker.Make (Irmin.Schema.KV (Contents))
module Store = Irmin_rtc.Make (Persistent_store)
module Sync = Irmin.Sync.Make (Store)

let basic_setup =
  Jv.get Jv.global "__CM__basic_setup" |> Code_mirror.Extension.of_jv

let init ?doc ?(exts = [||]) () =
  let open Code_mirror in
  let open Editor in
  let config =
    State.Config.create ?doc
      ~extensions:(Array.concat [ [| basic_setup |]; exts ])
      ()
  in
  let state = State.create ~config () in
  match find_element "text-editor" with
  | Some parent ->
      let opts = View.opts ~state ~parent () in
      let view : View.t = View.create ~opts () in
      (state, view)
  | _ -> failwith "Failed to find parent or markdown view."

let info msg () =
  let i = Unix.gettimeofday () |> Int64.of_float in
  Store.Info.v ~message:msg i

(* RTC Signalling *)

let peer_connection =
  let ice_server =
    Rtc.IceServer.of_jv
    @@ Jv.obj [| ("urls", Jv.of_string "stun:stun.1.google.com:19302") |]
  in
  let ice_servers = [| ice_server |] in
  let opts = Rtc.PeerConnection.config ~ice_servers () in
  Rtc.PeerConnection.create ~opts ()

let data_channel =
  let opts = Rtc.DataChannel.opts ~negotiated:true ~id:0 () in
  Rtc.PeerConnection.create_data_channel ~label:"irmin-rtc" ~opts
    peer_connection

let () =
  Rtc.DataChannel.Ev.set_on_message (fun m -> Console.log [ m ]) data_channel

let await_exn f =
  Fut.await f @@ function Error e -> Console.error [ e ] | Ok () -> ()

let create_offer offer pc =
  let run =
    let open Fut.Result_syntax in
    let* offer = Rtc.PeerConnection.create_offer pc in
    Console.log [ offer ];
    Rtc.PeerConnection.set_local_description offer pc
  in
  let (_ : Ev.listener) =
    Rtc.PeerConnection.Ev.set_on_ice_candidate
      (fun _ev ->
        Console.log [ "EEEFREF" ];
        match Rtc.PeerConnection.get_local_description pc with
        | None -> Console.log [ "No local description" ]
        | Some sdp ->
            let sdp = Rtc.PeerConnection.Sd.get_sdp sdp in
            set_value offer sdp)
      pc
  in
  Console.log [ pc ];
  await_exn run

let _handle_change () =
  let ice_state = Rtc.PeerConnection.get_ice_connection_state peer_connection in
  let state = Rtc.PeerConnection.get_signaling_state peer_connection in
  Console.log [ "Handling change:"; ice_state; state ]

let sync_store store (dc : Rtc.DataChannel.t) =
  let open Lwt.Infix in
  Console.log [ "Sync store" ];
  Sync.pull_exn store (Store.E dc) `Set >>= fun status ->
  Store.get store [ "index.md" ] >|= fun c -> Console.log [ status; c ]

let setup_rtc () =
  match
    ( find_element "offer-btn",
      find_element "sync-btn",
      find_element "offer",
      find_element "answer" )
  with
  | Some btn, Some sync, Some offer, Some answer ->
      let (_ : Ev.listener) =
        Ev.listen Ev.keydown
          (fun e ->
            let ev = Ev.as_type e in
            if
              Ev.Keyboard.code ev <> Jstr.v "Enter"
              || Rtc.PeerConnection.get_signaling_state peer_connection
                 <> Stable
            then ()
            else (
              El.set_at At.Name.disabled (Some (Jstr.v "true")) btn;
              let (_ : Ev.listener) =
                Rtc.PeerConnection.Ev.set_on_ice_candidate
                  (fun _ev ->
                    match
                      Rtc.PeerConnection.get_local_description peer_connection
                    with
                    | None -> ()
                    | Some c ->
                        set_value answer (Rtc.PeerConnection.Sd.get_sdp c))
                  peer_connection
              in
              let run =
                let open Fut.Result_syntax in
                let sdp = get_value offer in
                let sdp =
                  Jv.obj
                    [|
                      ("type", Jv.of_string "offer"); ("sdp", Jv.of_jstr sdp);
                    |]
                  |> Rtc.PeerConnection.Sd.of_jv
                in
                let* () =
                  Rtc.PeerConnection.set_remote_description sdp peer_connection
                in
                let* answer =
                  Rtc.PeerConnection.create_answer peer_connection
                in
                let+ () =
                  Rtc.PeerConnection.set_local_description answer
                    peer_connection
                in
                ()
              in
              await_exn run))
          (El.as_target offer)
      in
      let (_ : Ev.listener) =
        Ev.listen Ev.click
          (fun _ -> create_offer offer peer_connection)
          (El.as_target btn)
      in
      let (_ : Ev.listener) =
        Ev.listen Ev.keydown
          (fun ev ->
            let ev = Ev.as_type ev in
            if
              Ev.Keyboard.code ev <> Jstr.v "Enter"
              || Rtc.PeerConnection.get_signaling_state peer_connection
                 <> HaveLocalOffer
            then ()
            else
              let sdp = get_value answer in
              let sdp =
                Rtc.PeerConnection.Sd.of_jv
                  (Jv.obj
                     [|
                       ("type", Jv.of_string "answer"); ("sdp", Jv.of_jstr sdp);
                     |])
              in
              let (_ : _ Fut.t) =
                Rtc.PeerConnection.set_remote_description sdp peer_connection
              in
              ())
          (El.as_target answer)
      in
      let pull store =
        Ev.listen Ev.click
          (fun _ -> Lwt.async @@ fun () -> sync_store store data_channel)
          (El.as_target sync)
      in
      (offer, pull, answer)
  | _ -> failwith "Couldn't find offer and answer elements"

let main () =
  let open Code_mirror in
  let open Lwt.Syntax in
  let _offer, pull, _answer = setup_rtc () in
  match find_element "markdown" with
  | Some md ->
      let* store = Store.Repo.v (Irmin_indexeddb.config "rtc-irmin") in
      let* main = Store.main store in
      (* Initialise the pull mechanism *)
      let (_ : Ev.listener) = pull main in
      Store.rtc_serve main data_channel;
      let (Facet ((module F), facet)) = Editor.View.update_listener () in
      let buf = Buffer.create 128 in
      let view_update (view : Editor.View.Update.t) =
        Lwt.async @@ fun () ->
        let state =
          Editor.View.Update.to_jv view |> fun v ->
          Jv.get v "state" |> Editor.State.of_jv
        in
        let text = Editor.State.doc state in
        (* TODO: perhaps not every keystroke! *)
        let+ () =
          Store.set_exn ~info:(info "checkpoint") main [ "index.md" ]
            (Contents.of_text text)
        in
        let text = Text.to_jstr_array text in
        let text =
          Array.iter
            (fun j ->
              Buffer.add_string buf (Jstr.to_string j);
              Buffer.add_string buf "\n")
            text;
          let s = Buffer.contents buf in
          Buffer.clear buf;
          s
        in
        set_innerhtml md
          (Cmarkit.Doc.of_string text |> Cmarkit_html.of_doc ~safe:true)
      in
      let ext = F.of_ facet view_update in
      let+ saved = Store.find main [ "index.md" ] in
      Brr.Console.log [ saved ];
      let doc =
        Option.map (fun s -> String.concat "\n" s |> Jstr.of_string) saved
      in
      let _state, _view = init ?doc ~exts:[| ext |] () in
      ()
  | _ -> failwith "No markdown viewer"

let () = ignore (main () : unit Lwt.t)
