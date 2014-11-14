exception Codegen_error of string

let codegen_error msg = raise (Codegen_error msg)

(* code generation *)
let chan = ref stdout
let vars = ref (Hashtbl.create 100)

let set_chan new_chan = chan := new_chan

let gen v = output_string !chan v; output_string !chan "\n"

let bottom_var () = Hashtbl.fold (fun _ v c -> if v >= c then (v+4) else c) !vars 0
let empty_var i = (bottom_var ())+(4*(i-1))

let var_addr v = if String.length v > 6 && String.sub v 0 6 = "__temp" 
                 then let i = String.sub v 6 ((String.length v) - 6) in "[esp+" ^ i ^ "]"
                 else
                 try "[esp+" ^ string_of_int (Hashtbl.find !vars v) ^ "]"
                 with Not_found -> codegen_error ("identifier " ^ v ^ " not defined")
let var v = "dword " ^ (var_addr v)
let is_var v = 
    let re = Str.regexp_string "[esp+" in
    try ignore (Str.search_forward re v 0); true
    with Not_found -> false

let temp_var i = "__temp" ^ (string_of_int (empty_var i))

let is_alloc_var v = Hashtbl.mem !vars v

let alloc_var v = if is_alloc_var v then var v
                  else let _ = Hashtbl.replace !vars v (empty_var 1) in var v

let op opcode a = gen ("    " ^ opcode ^ "  " ^ a)
let op2 opcode a b = gen ("    " ^ opcode ^ "  " ^ a ^ ", " ^ b)
let push a = op "push" a

let generate_begin () = gen
"extern printf
extern scanf

section .data
    inf: db '%d', 0
    ouf: db '%d', 10, 0

section .text
    global main

main:
    sub   esp, 4096"

let generate_end () = gen
"    add   esp, 4096
exit:
    mov  eax, 1 ; sys_exit
    mov  ebx, 0
    int  80h"

let generate_read i = op2 "lea" "eax" (var_addr i); 
                      push "eax"; 
                      push "inf"; 
                      op "call" "scanf";
                      op2 "add " "esp" "8"

let generate_reads = List.iter generate_read

let generate_write i = push i; 
                       push "ouf"; 
                       op "call" "printf";
                       op2 "add " "esp" "8"

let generate_writes = List.iter generate_write

let generate_literal = string_of_int

let generate_copy a b = if a = b then () else
                        if is_var b then begin op2 "mov " "eax" b; op2 "mov " a "eax" end
                        else op2 "mov " a b
    
let generate_assign a b = generate_copy (alloc_var a) b

let generate_add d id1 id2 = let v = var (temp_var d) in
                             generate_copy v id1; 
                             if is_var id2 then begin op2 "mov " "eax" id2; op2 "add " v "eax" end
                             else op2 "add " v id2; v

let generate_sub d id1 id2 = let v = var (temp_var d) in
                             generate_copy v id1; 
                             if is_var id2 then begin op2 "mov " "eax" id2; op2 "sub " v "eax" end
                             else op2 "sub " v id2; v
    
