(* OxidizeLib - Modal Memory Management for OCaml 5 *)

module Modes = Modes
module Locality = Locality
module Uniqueness = Uniqueness
module Modality = Modality

module Pointer = struct
  type stack = private STACK_PTR
  type heap = private HEAP_PTR
  
  type _ ptr =
    | Stack : int -> stack ptr
    | Heap : int -> heap ptr
  
  let next_addr = ref 0
  let fresh_addr () = let addr = !next_addr in incr next_addr; addr
  
  let stack_alloc () : stack ptr = Stack (fresh_addr ())
  let heap_alloc () : heap ptr = Heap (fresh_addr ())
  
  let addr_of : type m. m ptr -> int = function
    | Stack n -> n
    | Heap n -> n
  
  (* Can't pass stack ptr here - GADT enforces it *)
  let free_heap : heap ptr -> unit = fun _ -> ()
end

module AffineClosure = struct
  open Uniqueness
  
  type ('a, 'b) t = {
    mutable called : bool;
    f : 'a -> 'b;
    name : string;
  }
  
  exception AlreadyCalled of string
  
  let make ?(name="<closure>") f = { called = false; f; name }
  
  let call c x =
    if c.called then raise (AlreadyCalled c.name)
    else begin c.called <- true; c.f x end
  
  let is_consumed c = c.called
    
  let capture_unique u f =
    make (fun () -> f (consume u))
end

module InPlace = struct
  open Uniqueness
  
  let update = update_inplace
  let update' u f = update f u
  let update_then_consume f u = consume (update f u)
    
  let swap u1 u2 =
    let v1 = consume u1 in
    let v2 = consume u2 in
    (make_unique v2, make_unique v1)
end

module Region = struct
  open Locality
  
  let run f = f ()
  let with_local : 'a -> ('a, 'b) region_scope -> 'b = with_region
  let alloc_locals xs f = f (make_local xs)
end

module Examples = struct
  open Uniqueness
  open Locality
  open Modality
  
  let reverse_example () =
    let xs = make_ulist [1; 2; 3; 4; 5] in
    consume_ulist (reverse_inplace xs)
  
  let affine_closure_example () =
    let xs = make_ulist [1; 2; 3] in
    let f = AffineClosure.capture_unique xs list_of_unique_list in
    AffineClosure.call f ()
  
  let local_example () =
    with_region [1; 2; 3; 4; 5] {
      run = fun local_data ->
        List.fold_left (+) 0 (get_local local_data)
    }
  
  let aliased_elements_example () =
    let nodes = make_unique_aliased_list ["node_a"; "node_b"; "node_c"] in
    consume_unique_aliased (reverse_unique_aliased nodes)
  
  let borrow_example () =
    let data = make_unique [1; 2; 3; 4; 5] in
    let (data', length) = borrow data List.length in
    let data'' = update_inplace (List.map (( * ) 2)) data' in
    (consume data'', length)
  
  let pointer_safety_example () =
    let stack_ptr = Pointer.stack_alloc () in
    let heap_ptr = Pointer.heap_alloc () in
    Pointer.free_heap heap_ptr;
    Pointer.addr_of stack_ptr
end

(* Re-exports *)
let make_unique = Uniqueness.make_unique
let consume = Uniqueness.consume
let peek = Uniqueness.peek
let borrow = Uniqueness.borrow

let with_region = Locality.with_region
let local_alloc = Locality.local_alloc
let local_get = Locality.local_get

let box_aliased = Modality.box_aliased
let box_many = Modality.box_many
let box_global = Modality.box_global
