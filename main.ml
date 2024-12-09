open Js_of_ocaml
open Js_of_ocaml_lwt

module Geo = struct
  let store_in_storage key value =
    Js.Optdef.iter (Dom_html.window##.localStorage)
      (fun storage -> storage##setItem (Js.string key) (Js.string value))

  let get_from_storage key =
    Js.Optdef.case (Dom_html.window##.localStorage)
      (fun () -> None)
      (fun storage ->
        Js.Opt.case (storage##getItem (Js.string key))
          (fun () -> None)
          (fun v -> Some (Js.to_string v)))

  (* UI Update Functions *)
  let update_ui city area tz lat long =
    let open Dom_html in
    let doc = window##.document in
    Js.Opt.iter (doc##getElementById(Js.string "location-info"))
      (fun container ->
        container##.innerHTML := Js.string (Printf.sprintf
          {|<div class="location-found">
             <p class="status-ok">Location found!</p>
             <p>City: %s</p>
             <p>Area: %s</p>
             <p>Time Zone: %s</p>
             <p class="coordinates">(%f, %f)</p>
           </div>|}
          city area tz lat long))

  let update_error_ui status =
    let open Dom_html in
    let doc = window##.document in
    Js.Opt.iter (doc##getElementById(Js.string "location-info"))
      (fun container ->
        let message = match status with
          | "denied" -> "Location access denied"
          | "unavail" -> "Location unavailable"
          | "timeout" -> "Request timed out"
          | "unsupported" -> "Geolocation not supported"
          | _ -> "Error getting location"
        in
        container##.innerHTML := Js.string (Printf.sprintf
          {|<div class="location-error">
             <p class="status-error">%s</p>
           </div>|}
          message))

  let geo () =
    if (Geolocation.is_supported()) then
      let success pos =
        let coords = pos##.coords in
        let latitude' = coords##.latitude in
        let longitude' = coords##.longitude in
        let city' = ref "" in
        let area' = ref "" in
        let tz' = ref "" in
        let dis = ref 90.0 in
        
        Hashtbl.iter (fun tz lst -> 
          List.iter (fun (city,area,lat,long) ->
            let dis' = (lat-.latitude')**2. +. (long-.longitude')**2. in
            if !dis > dis' then (
              dis := dis';
              city' := city;
              area' := area;
              tz' := tz
            )
          ) lst
        ) Base_locations.loch;
        
        store_in_storage "latitude" (string_of_float latitude');
        store_in_storage "longitude" (string_of_float longitude');
        store_in_storage "city" !city';
        store_in_storage "area" !area';
        store_in_storage "TZ" !tz';
        store_in_storage "status" "OK";
        
        update_ui !city' !area' !tz' latitude' longitude'
      in
      
      let error err =
        let status = 
          if err##._PERMISSION_DENIED_ = err##.code then "denied"
          else if err##._POSITION_UNAVAILABLE_ = err##.code then "unavail"
          else if err##._TIMEOUT = err##.code then "timeout"
          else "error"
        in
        store_in_storage "status" status;
        update_error_ui status
      in

      let options = Geolocation.empty_position_options() in
      options##.enableHighAccuracy := true;
      options##.timeout := 5000;
      options##.maximumAge := 0;
      
      Geolocation.geolocation##getCurrentPosition 
        (Js.wrap_callback success)
        (Js.wrap_callback error)
        options
    else (
      store_in_storage "status" "unsupported";
      update_error_ui "unsupported"
    )
end

let register_service_worker () =
  let open Js.Unsafe in
  if Js.Optdef.test (global##.navigator##.serviceWorker) then
    let sw = global##.navigator##.serviceWorker in
    ignore (sw##register (Js.string "/service-worker.js"))

(* Initialize application *)
let init () =
  register_service_worker ();
  Geo.geo ()

let () = 
  Dom_html.window##.onload := Dom_html.handler (fun _ ->
    init ();
    Js._false)
