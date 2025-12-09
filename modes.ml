(* Phantom types for modal memory management *)

module Affinity = struct
  type many = private MANY
  type once = private ONCE
  
  type _ witness =
    | Many : many witness
    | Once : once witness
end

module Uniqueness = struct
  type unique = private UNIQUE
  type aliased = private ALIASED
  
  type _ witness =
    | Unique : unique witness
    | Aliased : aliased witness
end

module Locality = struct
  type global = private GLOBAL
  type local = private LOCAL
  
  type _ witness =
    | Global : global witness
    | Local : local witness
end

type ('a, 'u, 'l) mode = {
  affinity : 'a Affinity.witness;
  uniqueness : 'u Uniqueness.witness;
  locality : 'l Locality.witness;
}

let legacy : (Affinity.many, Uniqueness.aliased, Locality.global) mode = {
  affinity = Affinity.Many;
  uniqueness = Uniqueness.Aliased;
  locality = Locality.Global;
}

(* Submoding: unique < aliased, many < once, global < local *)

module UniqueSubmode = struct
  type ('a, 'b) sub =
    | Refl : ('a, 'a) sub
    | UniqueToAliased : (Uniqueness.unique, Uniqueness.aliased) sub
    
  let forget_uniqueness = UniqueToAliased
end

module AffinitySubmode = struct
  type ('a, 'b) sub =
    | Refl : ('a, 'a) sub
    | ManyToOnce : (Affinity.many, Affinity.once) sub
    
  let restrict_to_once = ManyToOnce
end

module LocalitySubmode = struct
  type ('a, 'b) sub =
    | Refl : ('a, 'a) sub  
    | GlobalToLocal : (Locality.global, Locality.local) sub
    
  let make_local = GlobalToLocal
end
