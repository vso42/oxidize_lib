(* Uniqueness via GADTs + destructive moves *)

type alive = private ALIVE
type consumed = private CONSUMED

type _ state_witness =
  | Alive : alive state_witness
  | Consumed : consumed state_witness

exception UniqueConsumed of string

type ('s, 'a) unique_ref = {
  mutable contents : 'a option;
  name : string;
}

type 'a unique = (alive, 'a) unique_ref
type 'a consumed_ref = (consumed, 'a) unique_ref

let make_unique ?(name="<anon>") (x : 'a) : 'a unique = 
  { contents = Some x; name }

let make_unique_named name x = { contents = Some x; name }

let is_alive : ('s, 'a) unique_ref -> bool =
  fun r -> Option.is_some r.contents

let read_aliased : 'a unique -> 'a = fun r ->
  match r.contents with
  | Some x -> x
  | None -> raise (UniqueConsumed r.name)

let peek = read_aliased

(* Destructive move - invalidates all aliases *)
let consume : 'a unique -> 'a = fun r ->
  match r.contents with
  | Some x -> r.contents <- None; x
  | None -> raise (UniqueConsumed r.name)

let consume_map f r = f (consume r)

let update_inplace : ('a -> 'a) -> 'a unique -> 'a unique = fun f r ->
  match r.contents with
  | Some x -> r.contents <- Some (f x); r
  | None -> raise (UniqueConsumed r.name)

let replace_inplace : 'a -> 'a unique -> 'a unique = fun x r ->
  match r.contents with
  | Some _ -> r.contents <- Some x; r
  | None -> raise (UniqueConsumed r.name)

(* Space credits for memory reuse *)
type 'a space_credit = {
  mutable available : bool;
  phantom : 'a option;
}

let consume_for_credit : 'a unique -> 'a * 'a space_credit = fun r ->
  let value = consume r in
  (value, { available = true; phantom = None })

let reuse_credit : 'a space_credit -> 'a -> 'a unique = fun credit value ->
  if credit.available then begin
    credit.available <- false;
    make_unique value
  end else
    failwith "Space credit already used"

type ('a, 'b) unique_pair = ('a unique * 'b unique) unique

let make_unique_pair a b = make_unique (make_unique a, make_unique b)

let split_unique_pair p = consume p

let project_fst : ('a, 'b) unique_pair -> 'a * ('a, 'b) unique_pair = fun p ->
  let (ua, _ub) = peek p in
  (consume ua, p)

(* Unique lists with in-place operations *)
type 'a unique_list =
  | UNil
  | UCons of { mutable hd : 'a; mutable tl : 'a unique_list }

type 'a ulist = 'a unique_list unique

let rec unique_list_of_list = function
  | [] -> UNil
  | x :: xs -> UCons { hd = x; tl = unique_list_of_list xs }

let make_ulist xs = make_unique (unique_list_of_list xs)

let reverse_inplace : 'a ulist -> 'a ulist = fun ul ->
  let rec rev_append prev = function
    | UNil -> prev
    | UCons cell ->
      let next = cell.tl in
      cell.tl <- prev;
      rev_append (UCons cell) next
  in
  update_inplace (fun l -> rev_append UNil l) ul

let rec list_of_unique_list = function
  | UNil -> []
  | UCons { hd; tl } -> hd :: list_of_unique_list tl

let consume_ulist ul = list_of_unique_list (consume ul)

(* Borrowing: temporarily alias, then regain uniqueness *)
let borrow : 'a unique -> ('a -> 'b) -> 'a unique * 'b = fun r f ->
  match r.contents with
  | Some x -> (r, f x)
  | None -> raise (UniqueConsumed r.name)

let with_borrow r f cont =
  let (r', result) = borrow r f in
  cont r' result

(* State machine via GADTs *)
type fresh = FRESH
type in_use = IN_USE

type (_, _) transition =
  | FreshToUse : (fresh, in_use) transition
  | UseToConsumed : (in_use, consumed) transition
  | FreshToConsumed : (fresh, consumed) transition

module LinearResource : sig
  type ('state, 'a) t
  val create : 'a -> (fresh, 'a) t
  val use : (fresh, 'a) t -> (in_use, 'a) t * 'a
  val finish : (in_use, 'a) t -> (consumed, 'a) t
  val consume_direct : (fresh, 'a) t -> (consumed, 'a) t * 'a
end = struct
  type ('state, 'a) t = {
    mutable value : 'a option;
    mutable state : [`Fresh | `InUse | `Consumed];
  }
  
  let create x = { value = Some x; state = `Fresh }
  
  let use r = match r.state, r.value with
    | `Fresh, Some v -> r.state <- `InUse; ({ r with state = `InUse }, v)
    | _ -> failwith "Invalid state transition"
    
  let finish r = match r.state with
    | `InUse -> r.state <- `Consumed; r.value <- None; { r with state = `Consumed }
    | _ -> failwith "Invalid state transition"
    
  let consume_direct r = match r.state, r.value with
    | `Fresh, Some v -> r.state <- `Consumed; r.value <- None; ({ r with state = `Consumed }, v)
    | _ -> failwith "Invalid state transition"
end
