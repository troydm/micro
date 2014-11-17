(* compiling *)
let compile f = 
        let out = (Filename.chop_extension f) in
        let out_chan = open_out (out ^ ".s")
        and lexbuf = Lexing.from_channel (open_in f) in
        try
            let parse () = Parser.program Lexer.micro lexbuf in
            Codegen.set_chan out_chan;
            Codegen.codegen (parse ());
            close_out out_chan;
            ignore(Sys.command ("nasm -f elf32 " ^ out ^ ".s"));
            ignore(Sys.command ("gcc -m32 -o " ^ out ^ " " ^ out ^ ".o"))
        with 
        | Codegen.Codegen_error s ->
            print_string s;
            print_string "\n";
            exit 1
        | Lexer.Syntax_error s ->
            print_string s;
            print_string "\n";
            exit 1

let help () = print_string "micro <file>\n"

let () = if Array.length Sys.argv = 1 then help ()
         else 
             let file = Array.get Sys.argv 1 in
             Format.printf "compiling %s\n" file;
             Format.print_flush ();
             compile file

