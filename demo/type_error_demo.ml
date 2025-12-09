(* Uncomment ERROR lines to see compile-time safety *)

open OxidizeLib

let pointer_safety () =
  let stack_ptr = Pointer.stack_alloc () in
  let heap_ptr = Pointer.heap_alloc () in
  Pointer.free_heap heap_ptr;
  
  (* ERROR: Pointer.free_heap stack_ptr
     Type stack is not compatible with type heap *)
  
  ignore stack_ptr

let locality_safety () =
  let _ = Locality.with_region "hello" {
    run = fun local_ref ->
      String.length (Locality.get_local local_ref)
      (* ERROR: local_ref  -- can't return, 'r escapes *)
  } in
  ()

let state_machine_safety () =
  let r = Uniqueness.LinearResource.create 42 in
  let (r', _) = Uniqueness.LinearResource.use r in
  let _ = Uniqueness.LinearResource.finish r' in
  (* ERROR: Uniqueness.LinearResource.finish r' -- already finished *)
  ()

let () =
  Printf.printf "Compiles OK. Uncomment ERROR lines for type errors.\n";
  pointer_safety ();
  locality_safety ();
  state_machine_safety ()
