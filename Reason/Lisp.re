

type expr =
  | LispInt of int
  | LispSymbol of string
  | LispList of (list expr)
;


let rec unwords = fun lst =>
  switch lst {
    | [] => ""
    | x::[] => x
    | x::xs => x ^ " " ^ (unwords xs)
  };

let rec to_string = fun exp => 
  switch exp { 
    | LispInt a => "Int:" ^ string_of_int a
    | LispSymbol s => "Str:\"" ^ s ^ "\""
    | LispList lst => "List:[" ^ (unwords (List.map to_string lst)) ^ "]"
  };
/*
print_string ( (to_string (LispInt 1)) ^ "\n" );
print_string ( (to_string (LispSymbol "hoge")) ^ "\n" );
print_string ( (to_string (LispList [(LispInt 1), (LispSymbol "a")])) ^ "\n" );
*/



let parse = {
  let rec add_space = fun s =>
    switch s {
      | "" => ""
      | x::xs => if (x == "(" || x == ")") {
                   " " ^ x ^ " " ^ (add_space xs)
                 } else {
                   x ^ (add_space xs)
                 }
    };

  let words = {
    let rec split_words = fun (acc, str) =>
      switch str {
        | "" => [List.rev acc]
        | " "::xs => (List.rev acc) :: (split_words ""  xs)
        | x::xs => split_words (x ^ )  xs
      };


      fun s =>
      switch s {
        | "" => []
        | " "::ss => []
        | "" => []
      };
  };

  let tokenize = fun s =>
    switch s {
      | "" => []
      | s = words (add_space s)
    };
  fun src => fst ;
};


/*
print_string ( (to_string (parse "(if t (quote 1) (quote 3))")) ^ "\n" );
*/


