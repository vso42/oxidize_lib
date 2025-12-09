# OxidizeLib

Modal memory management for OCaml 5, without compiler mods.

Based on: "Oxidizing OCaml with Modal Memory Management" (Lorenzen et al., ICFP 2024)

## What it does

Uses standard OCaml type features to enforce memory safety:

- **Phantom types + GADTs**: Stack pointers can't be freed as heap
- **Rank-2 polymorphism**: Local values can't escape regions  
- **Destructive moves**: Aliased access to consumed values fails safely

## The three modes

| Axis | Values | Controls |
|------|--------|----------|
| Locality | global / local | Can value escape region? |
| Uniqueness | unique / aliased | Safe for in-place update? |
| Affinity | many / once | How many times usable? |

## Quick examples

```ocaml
(* Stack vs heap - compile-time safety *)
let sp = Pointer.stack_alloc () in
Pointer.free_heap sp  (* TYPE ERROR *)

(* In-place list reversal *)
let xs = make_ulist [1;2;3;4;5] in
consume_ulist (reverse_inplace xs)  (* [5;4;3;2;1] *)

(* Local values can't escape *)
with_region [1;2;3] { run = fun local ->
  List.length (get_local local)  (* OK: int escapes *)
  (* local *)  (* ERROR: local_ref can't escape *)
}

(* Borrow then regain uniqueness *)
let (u', len) = borrow unique_data List.length in
(* u' is still unique *)
```

## Build

```bash
dune build
dune exec test/test_oxidize.exe
dune exec demo/demo.exe
```

## Structure

```
lib/
  modes.ml       - phantom types
  locality.ml    - rank-2 regions
  uniqueness.ml  - GADTs + destructive moves
  modality.ml    - aliased/many/global boxes
  oxidizeLib.ml  - main interface
```

## Limitations

Can't prevent aliasing statically (`let y = x` always works).
Uses runtime checks as fallback - raises `UniqueConsumed` on misuse.

No actual stack allocation - just type discipline.

## License

MIT
