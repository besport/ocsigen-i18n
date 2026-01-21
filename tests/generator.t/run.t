  $ ocsigen-i18n-generator --languages=en,fr-fr < tr.tsv
  [%%shared type t = En|Fr_fr]
  [%%shared exception Unknown_language of string]
  let%shared string_of_language = function 
  | En -> "en"| Fr_fr -> "fr-fr"
  let%shared language_of_string = function
  | "en" -> En| "fr-fr" -> Fr_fr| s -> raise (Unknown_language s)
  let%shared guess_language_of_string s = 
  try language_of_string s 
  with Unknown_language _ as e -> 
  try language_of_string (String.sub s 0 (String.index s '-')) 
  with Not_found -> 
  raise e 
  let%shared languages = [En;Fr_fr]
  let%shared default_language = En
  let%server _language_ = Eliom_reference.Volatile.eref
  ~scope:Eliom_common.default_process_scope default_language
  let%server get_language () = Eliom_reference.Volatile.get _language_
  let%server set_language language = 
  Eliom_reference.Volatile.set _language_ language
  
  let%client _language_ = ref default_language
  let%client get_language () = !_language_
  let%client set_language language = _language_ := language
  
  let%shared txt = Eliom_content.Html.F.txt
  [%%shared
  module Tr = struct
  let foo ?(lang = get_language ()) ()  () =
  match lang with
  | En -> [txt "This is a simple key."]
  | Fr_fr -> [txt "Ceci est une cl\195\169 toute simple."]
  let bar ?(lang = get_language ()) () ~x () =
  match lang with
  | En -> List.flatten [[txt "I am "];x;[txt "."]]
  | Fr_fr -> List.flatten [[txt "Je suis "];x;[txt "."]]
  let baz ?(lang = get_language ()) () ?(c=false) () =
  match lang with
  | En -> List.flatten [[txt "There "];[txt (if c then "are" else "is")];[txt " apple"];[txt (if c then "s" else "")];[txt " here!"]]
  | Fr_fr -> List.flatten [[txt "Il y a "];[txt (if c then "des" else "une")];[txt " pomme"];[txt (if c then "s" else "")];[txt " ici !"]]
  let bu ?(lang = get_language ()) () ~n ~x () =
  match lang with
  | En -> List.flatten [[txt "I am "];[txt (Printf.sprintf "%s" x)];[txt " ("];[txt (Printf.sprintf "%d" n)];[txt ")."]]
  | Fr_fr -> List.flatten [[txt "Je suis "];[txt (Printf.sprintf "%s" x)];[txt " ("];[txt (Printf.sprintf "%d" n)];[txt ")."]]
  module S = struct
  let foo ?(lang = get_language ()) ()  () =
  match lang with
  | En -> "This is a simple key."
  | Fr_fr -> "Ceci est une cl\195\169 toute simple."
  let bar ?(lang = get_language ()) () ~x () =
  match lang with
  | En -> String.concat "" ["I am ";x;"."]
  | Fr_fr -> String.concat "" ["Je suis ";x;"."]
  let baz ?(lang = get_language ()) () ?(c=false) () =
  match lang with
  | En -> String.concat "" ["There ";(if c then "are" else "is");" apple";(if c then "s" else "");" here!"]
  | Fr_fr -> String.concat "" ["Il y a ";(if c then "des" else "une");" pomme";(if c then "s" else "");" ici !"]
  let bu ?(lang = get_language ()) () ~n ~x () =
  match lang with
  | En -> String.concat "" ["I am ";(Printf.sprintf "%s" x);" (";(Printf.sprintf "%d" n);")."]
  | Fr_fr -> String.concat "" ["Je suis ";(Printf.sprintf "%s" x);" (";(Printf.sprintf "%d" n);")."]
  end
  end
  ]
