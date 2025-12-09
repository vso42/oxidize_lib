(* Locality via Rank-2 Polymorphism - prevents local values from escaping *)

type ('r, 'a) local_ref = { data : 'a }

let make_local (x : 'a) : ('r, 'a) local_ref = { data = x }
let get_local (r : ('r, 'a) local_ref) : 'a = r.data

(* The 'r is universally quantified, so it can't appear in return type *)
type ('a, 'b) region_scope = {
  run : 'r. ('r, 'a) local_ref -> 'b
}

let with_region : 'a -> ('a, 'b) region_scope -> 'b =
  fun init scope ->
    let local_val = make_local init in
    scope.run local_val

let run_region : (unit, 'a) region_scope -> 'a =
  fun scope -> scope.run (make_local ())

type ('r, 'a) local = ('r, 'a) local_ref

let local_alloc : 'a -> ('r, 'a) local = make_local
let local_get : ('r, 'a) local -> 'a = get_local

let local_map : ('a -> 'b) -> ('r, 'a) local -> ('r, 'b) local =
  fun f loc -> { data = f loc.data }

type ('a, 'b) nested_scope = {
  run_nested : 's. ('s, 'a) local_ref -> 'b
}

let with_nested_region : 'a -> ('a, 'b) nested_scope -> 'b =
  fun init scope -> scope.run_nested (make_local init)

let local_pair : ('r, 'a) local -> ('r, 'b) local -> ('r, 'a * 'b) local =
  fun a b -> { data = (a.data, b.data) }

let local_fst : ('r, 'a * 'b) local -> ('r, 'a) local =
  fun p -> { data = fst p.data }

let local_snd : ('r, 'a * 'b) local -> ('r, 'b) local =
  fun p -> { data = snd p.data }

type ('r, 'a, 'b) local_fn = ('r, 'a -> 'b) local

let local_apply : ('r, 'a, 'b) local_fn -> 'a -> ('r, 'b) local =
  fun f x -> { data = f.data x }

let local_closure : ('a -> 'b) -> ('r, 'a, 'b) local_fn =
  fun f -> { data = f }

type 'a global_safe = GlobalSafe of 'a
let unsafe_make_global : 'a -> 'a global_safe = fun x -> GlobalSafe x
let extract_global : 'a global_safe -> 'a = fun (GlobalSafe x) -> x

(* Stack-allocated lists *)
type ('r, 'a) local_list =
  | LNil : ('r, 'a) local_list
  | LCons : 'a * ('r, 'a) local_list -> ('r, 'a) local_list

let rec local_of_list : 'a list -> ('r, 'a) local_list = function
  | [] -> LNil
  | x :: xs -> LCons (x, local_of_list xs)

let rec local_list_fold : ('b -> 'a -> 'b) -> 'b -> ('r, 'a) local_list -> 'b =
  fun f acc -> function
    | LNil -> acc
    | LCons (x, xs) -> local_list_fold f (f acc x) xs

let local_list_length ll = local_list_fold (fun n _ -> n + 1) 0 ll

let rec local_list_map : ('a -> 'b) -> ('r, 'a) local_list -> ('r, 'b) local_list =
  fun f -> function
    | LNil -> LNil
    | LCons (x, xs) -> LCons (f x, local_list_map f xs)
