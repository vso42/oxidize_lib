open OxidizeLib

let print_section title =
  Printf.printf "\n%s\n%s\n" title (String.make (String.length title) '=')

let demo_pointer_safety () =
  print_section "Demo 1: Pointer Safety (Stack vs Heap)";
  
  let stack_ptr = Pointer.stack_alloc () in
  let heap_ptr = Pointer.heap_alloc () in
  
  Printf.printf "Stack pointer: %d\n" (Pointer.addr_of stack_ptr);
  Printf.printf "Heap pointer: %d\n" (Pointer.addr_of heap_ptr);
  
  Pointer.free_heap heap_ptr;
  Printf.printf "Heap freed. Stack can't be freed - enforced at compile time.\n";
  ignore stack_ptr

let demo_uniqueness () =
  print_section "Demo 2: Uniqueness and In-Place Update";
  
  let xs = Uniqueness.make_ulist [1; 2; 3; 4; 5] in
  Printf.printf "Original: [1; 2; 3; 4; 5]\n";
  
  let reversed = Uniqueness.reverse_inplace xs in
  let result = Uniqueness.consume_ulist reversed in
  Printf.printf "Reversed in-place: [%s]\n" 
    (String.concat "; " (List.map string_of_int result));
  
  Printf.printf "\nDestructive move:\n";
  let u = Uniqueness.make_unique_named "my_val" 42 in
  let alias = u in
  let v = Uniqueness.consume u in
  Printf.printf "Consumed: %d\n" v;
  
  begin try
    let _ = Uniqueness.peek alias in ()
  with Uniqueness.UniqueConsumed name ->
    Printf.printf "Alias blocked: UniqueConsumed(%s)\n" name
  end

let demo_locality () =
  print_section "Demo 3: Locality (Regions)";
  
  let sum = Locality.with_region [1; 2; 3; 4; 5] {
    run = fun local_ref ->
      let data = Locality.get_local local_ref in
      Printf.printf "Inside region: [%s]\n"
        (String.concat "; " (List.map string_of_int data));
      List.fold_left (+) 0 data
  } in
  Printf.printf "Sum escaped: %d\n" sum;
  
  let length = Locality.with_region () {
    run = fun _ ->
      Locality.local_list_length (Locality.local_of_list [10; 20; 30; 40; 50])
  } in
  Printf.printf "Local list length: %d\n" length

let demo_borrowing () =
  print_section "Demo 4: Borrowing";
  
  let data = Uniqueness.make_unique [10; 20; 30] in
  Printf.printf "Original: [10; 20; 30]\n";
  
  let (data', length) = Uniqueness.borrow data List.length in
  Printf.printf "Borrowed for length: %d\n" length;
  
  let data'' = Uniqueness.update_inplace (List.map (( * ) 2)) data' in
  let final = Uniqueness.consume data'' in
  Printf.printf "Doubled: [%s]\n" (String.concat "; " (List.map string_of_int final))

let demo_modalities () =
  print_section "Demo 5: Modalities";
  
  let nodes = Modality.make_unique_aliased_list ["A"; "B"; "C"; "D"] in
  Printf.printf "Unique list, aliased elements: [A; B; C; D]\n";
  
  let reversed = Modality.reverse_unique_aliased nodes in
  let result = Modality.consume_unique_aliased reversed in
  Printf.printf "Reversed: [%s]\n" (String.concat "; " result)

let demo_affine () =
  print_section "Demo 6: Affine Closures";
  
  let secret = Uniqueness.make_unique "secret_data" in
  let f = AffineClosure.capture_unique secret String.length in
  
  let result = AffineClosure.call f () in
  Printf.printf "First call: %d\n" result;
  
  begin try
    let _ = AffineClosure.call f () in ()
  with AffineClosure.AlreadyCalled _ ->
    Printf.printf "Second call blocked.\n"
  end

let () =
  Printf.printf "OxidizeLib Demo\n===============\n";
  demo_pointer_safety ();
  demo_uniqueness ();
  demo_locality ();
  demo_borrowing ();
  demo_modalities ();
  demo_affine ();
  Printf.printf "\nDone.\n"
