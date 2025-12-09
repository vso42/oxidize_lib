(* Modality boxes for adjusting modes *)

(* aliased: (a, u, l) -> (a, aliased, l) *)
type 'a aliased_box = { aliased_contents : 'a } [@@unboxed]

let box_aliased x = { aliased_contents = x }
let unbox_aliased box = box.aliased_contents
let map_aliased f box = { aliased_contents = f box.aliased_contents }

(* many: (a, u, l) -> (many, u, l) *)
type 'a many_box = { many_contents : 'a } [@@unboxed]

let box_many x = { many_contents = x }
let unbox_many box = box.many_contents
let map_many f box = { many_contents = f box.many_contents }

(* global: (a, u, l) -> (a, aliased, global) *)
type 'a global_box = { global_contents : 'a } [@@unboxed]

let box_global x = { global_contents = x }
let unbox_global box = box.global_contents
let map_global f box = { global_contents = f box.global_contents }

let compose_aliased_aliased box = 
  { aliased_contents = box.aliased_contents.aliased_contents }

let compose_many_many box = 
  { many_contents = box.many_contents.many_contents }

let compose_global_global box = 
  { global_contents = box.global_contents.global_contents }

module Aliased = struct
  type 'a t = 'a aliased_box
  let make = box_aliased
  let get = unbox_aliased
  let map = map_aliased
  let (!) = get
end

module Many = struct
  type 'a t = 'a many_box
  let make = box_many
  let get = unbox_many
  let map = map_many
  let (!) = get
end

module Global = struct
  type 'a t = 'a global_box
  let make = box_global
  let get = unbox_global
  let map = map_global
  let (!) = get
end

(* Lists with aliased elements *)
type 'a aliased_elem_list = 'a Aliased.t list

let aliased_list_of_list xs = List.map Aliased.make xs
let map_aliased_list f xs = List.map (Aliased.map f) xs
let list_of_aliased_list xs = List.map Aliased.get xs

open Uniqueness

(* Unique list structure, aliased elements *)
type 'a unique_aliased_list = 'a Aliased.t ulist

let make_unique_aliased_list xs = make_ulist (List.map Aliased.make xs)
let reverse_unique_aliased = reverse_inplace
let consume_unique_aliased ul = List.map Aliased.get (consume_ulist ul)

type ('a_mode, 'u_mode, 'l_mode, 'value) mode_box = { value : 'value }

module ModeBox = struct
  open Modes.Affinity
  open Modes.Uniqueness  
  open Modes.Locality
  
  type 'a legacy = (many, aliased, global, 'a) mode_box
  type 'a unique_global = (many, unique, global, 'a) mode_box
  type 'a local_aliased = (many, aliased, local, 'a) mode_box
  type 'a once_unique = (once, unique, global, 'a) mode_box
  
  let make_legacy x : 'a legacy = { value = x }
  let make_unique x : 'a unique_global = { value = x }
  let make_local x : 'a local_aliased = { value = x }
  let make_once_unique x : 'a once_unique = { value = x }
  let get box = box.value
end
