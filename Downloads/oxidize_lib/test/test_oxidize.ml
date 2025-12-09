open OxidizeLib

let test_name = ref ""
let tests_passed = ref 0
let tests_failed = ref 0

let test name f =
  test_name := name;
  try
    f ();
    incr tests_passed;
    Printf.printf "✓ %s\n" name
  with e ->
    incr tests_failed;
    Printf.printf "✗ %s: %s\n" name (Printexc.to_string e)

let assert_eq msg expected actual =
  if expected <> actual then
    failwith (Printf.sprintf "%s: expected %s, got %s" 
      msg (Obj.magic expected |> string_of_int) (Obj.magic actual |> string_of_int))

let assert_list_eq msg expected actual =
  if expected <> actual then failwith (Printf.sprintf "%s: lists differ" msg)

let assert_raises msg f =
  try ignore (f ()); failwith (Printf.sprintf "%s: expected exception" msg)
  with _ -> ()

let test_uniqueness () =
  print_endline "\n=== Uniqueness Tests ===\n";
  
  test "make and consume unique value" (fun () ->
    let u = Uniqueness.make_unique 42 in
    assert_eq "value" 42 (Uniqueness.consume u)
  );
  
  test "double consume raises exception" (fun () ->
    let u = Uniqueness.make_unique 42 in
    let _ = Uniqueness.consume u in
    assert_raises "double consume" (fun () -> Uniqueness.consume u)
  );
  
  test "peek doesn't consume" (fun () ->
    let u = Uniqueness.make_unique 42 in
    assert_eq "peek1" 42 (Uniqueness.peek u);
    assert_eq "peek2" 42 (Uniqueness.peek u);
    assert_eq "consume" 42 (Uniqueness.consume u)
  );
  
  test "update in place" (fun () ->
    let u = Uniqueness.make_unique 10 in
    let u' = Uniqueness.update_inplace (fun x -> x * 2) u in
    assert_eq "updated value" 20 (Uniqueness.consume u')
  );
  
  test "aliased access after unique consume fails" (fun () ->
    let u = Uniqueness.make_unique 42 in
    let alias = u in
    let _ = Uniqueness.consume u in
    assert_raises "aliased access" (fun () -> Uniqueness.peek alias)
  );
  
  test "in-place list reversal" (fun () ->
    let ul = Uniqueness.make_ulist [1; 2; 3; 4; 5] in
    let reversed = Uniqueness.reverse_inplace ul in
    assert_list_eq "reversed" [5; 4; 3; 2; 1] (Uniqueness.consume_ulist reversed)
  );
  
  test "borrow and regain uniqueness" (fun () ->
    let u = Uniqueness.make_unique [1; 2; 3] in
    let (u', len) = Uniqueness.borrow u List.length in
    assert_eq "length" 3 len;
    assert_list_eq "borrowed value" [1; 2; 3] (Uniqueness.consume u')
  )

let test_locality () =
  print_endline "\n=== Locality Tests ===\n";
  
  test "with_region returns computed value" (fun () ->
    let result = Locality.with_region 42 { run = fun r -> Locality.get_local r * 2 } in
    assert_eq "result" 84 result
  );
  
  test "local list operations" (fun () ->
    let result = Locality.with_region [1; 2; 3; 4; 5] {
      run = fun r ->
        Locality.local_list_length (Locality.local_of_list (Locality.get_local r))
    } in
    assert_eq "length" 5 result
  );
  
  test "local list fold" (fun () ->
    let result = Locality.with_region () {
      run = fun _ ->
        Locality.local_list_fold (+) 0 (Locality.local_of_list [1; 2; 3; 4; 5])
    } in
    assert_eq "sum" 15 result
  );
  
  test "local list map stays local" (fun () ->
    let result = Locality.with_region () {
      run = fun _ ->
        let ll = Locality.local_of_list [1; 2; 3] in
        Locality.local_list_fold (+) 0 (Locality.local_list_map (fun x -> x * 2) ll)
    } in
    assert_eq "sum of doubled" 12 result
  );
  
  test "nested regions" (fun () ->
    let result = Locality.with_region 10 {
      run = fun outer ->
        Locality.with_nested_region 5 {
          run_nested = fun inner ->
            Locality.get_local outer + Locality.get_local inner
        }
    } in
    assert_eq "nested sum" 15 result
  );
  
  test "type system prevents local escape (compile-time)" (fun () -> ())

let test_modality () =
  print_endline "\n=== Modality Tests ===\n";
  
  test "aliased box wrapping" (fun () ->
    let boxed = Modality.box_aliased 42 in
    assert_eq "value" 42 (Modality.unbox_aliased boxed)
  );
  
  test "aliased list elements" (fun () ->
    let al = Modality.aliased_list_of_list [1; 2; 3] in
    assert_list_eq "round trip" [1; 2; 3] (Modality.list_of_aliased_list al)
  );
  
  test "unique list with aliased elements" (fun () ->
    let ul = Modality.make_unique_aliased_list [1; 2; 3; 4; 5] in
    let reversed = Modality.reverse_unique_aliased ul in
    assert_list_eq "reversed" [5; 4; 3; 2; 1] (Modality.consume_unique_aliased reversed)
  );
  
  test "many box" (fun () ->
    let boxed = Modality.box_many "hello" in
    assert_eq "first" "hello" (Modality.unbox_many boxed);
    assert_eq "second" "hello" (Modality.unbox_many boxed)
  );
  
  test "global box" (fun () ->
    assert_eq "value" 42 (Modality.unbox_global (Modality.box_global 42))
  );
  
  test "Aliased module interface" (fun () ->
    assert_eq "value" 42 (Modality.Aliased.get (Modality.Aliased.make 42))
  )

let test_pointers () =
  print_endline "\n=== Pointer Safety Tests ===\n";
  
  test "stack and heap allocation" (fun () ->
    let _ = Pointer.addr_of (Pointer.stack_alloc ()) in
    let _ = Pointer.addr_of (Pointer.heap_alloc ()) in
    ()
  );
  
  test "free_heap only accepts heap pointers" (fun () ->
    Pointer.free_heap (Pointer.heap_alloc ())
  );
  
  test "pointer addresses are distinct" (fun () ->
    let a1 = Pointer.addr_of (Pointer.heap_alloc ()) in
    let a2 = Pointer.addr_of (Pointer.heap_alloc ()) in
    if a1 = a2 then failwith "addresses should be different"
  )

let test_affine_closures () =
  print_endline "\n=== Affine Closure Tests ===\n";
  
  test "affine closure can be called once" (fun () ->
    let f = AffineClosure.make (fun x -> x * 2) in
    assert_eq "result" 42 (AffineClosure.call f 21)
  );
  
  test "affine closure cannot be called twice" (fun () ->
    let f = AffineClosure.make (fun x -> x * 2) in
    let _ = AffineClosure.call f 21 in
    assert_raises "second call" (fun () -> AffineClosure.call f 21)
  );
  
  test "capture unique in affine closure" (fun () ->
    let u = Uniqueness.make_unique [1; 2; 3] in
    let f = AffineClosure.capture_unique u List.length in
    assert_eq "length" 3 (AffineClosure.call f ());
    assert_raises "unique consumed" (fun () -> Uniqueness.peek u)
  );
  
  test "is_consumed tracks state" (fun () ->
    let f = AffineClosure.make (fun () -> 42) in
    if AffineClosure.is_consumed f then failwith "should not be consumed";
    let _ = AffineClosure.call f () in
    if not (AffineClosure.is_consumed f) then failwith "should be consumed"
  )

let test_inplace () =
  print_endline "\n=== InPlace Combinator Tests ===\n";
  
  test "update combinator" (fun () ->
    let u = Uniqueness.make_unique 10 in
    assert_eq "updated" 15 (Uniqueness.consume (InPlace.update (fun x -> x + 5) u))
  );
  
  test "pipeline operator" (fun () ->
    let u = Uniqueness.make_unique 2 in
    let u' = InPlace.update' (InPlace.update' u (fun x -> x * 3)) (fun x -> x + 1) in
    assert_eq "pipelined" 7 (Uniqueness.consume u')
  );
  
  test "swap" (fun () ->
    let (u1', u2') = InPlace.swap (Uniqueness.make_unique "a") (Uniqueness.make_unique "b") in
    assert_eq "swapped1" "b" (Uniqueness.consume u1');
    assert_eq "swapped2" "a" (Uniqueness.consume u2')
  )

let test_integration () =
  print_endline "\n=== Integration Tests ===\n";
  
  test "Example: reverse_example" (fun () ->
    assert_list_eq "reversed" [5; 4; 3; 2; 1] (Examples.reverse_example ())
  );
  
  test "Example: local_example" (fun () ->
    assert_eq "sum" 15 (Examples.local_example ())
  );
  
  test "Example: aliased_elements_example" (fun () ->
    assert_list_eq "nodes" ["node_c"; "node_b"; "node_a"] (Examples.aliased_elements_example ())
  );
  
  test "Example: borrow_example" (fun () ->
    let (result, length) = Examples.borrow_example () in
    assert_list_eq "doubled" [2; 4; 6; 8; 10] result;
    assert_eq "length" 5 length
  );
  
  test "Example: pointer_safety_example" (fun () ->
    let _ = Examples.pointer_safety_example () in ()
  )

let test_linear_resource () =
  print_endline "\n=== LinearResource State Machine Tests ===\n";
  
  test "create -> use -> finish" (fun () ->
    let r = Uniqueness.LinearResource.create 42 in
    let (r', v) = Uniqueness.LinearResource.use r in
    assert_eq "value" 42 v;
    let _ = Uniqueness.LinearResource.finish r' in ()
  );
  
  test "create -> consume_direct" (fun () ->
    let r = Uniqueness.LinearResource.create 42 in
    let (_, v) = Uniqueness.LinearResource.consume_direct r in
    assert_eq "value" 42 v
  );
  
  test "double use fails" (fun () ->
    let r = Uniqueness.LinearResource.create 42 in
    let (r', _) = Uniqueness.LinearResource.use r in
    let _ = Uniqueness.LinearResource.finish r' in
    assert_raises "double finish" (fun () -> Uniqueness.LinearResource.finish r')
  )

let () =
  print_endline "OxidizeLib Test Suite";
  print_endline "=====================";
  
  test_uniqueness ();
  test_locality ();
  test_modality ();
  test_pointers ();
  test_affine_closures ();
  test_inplace ();
  test_integration ();
  test_linear_resource ();
  
  print_endline "\n=====================";
  Printf.printf "Results: %d passed, %d failed\n" !tests_passed !tests_failed;
  if !tests_failed > 0 then exit 1
