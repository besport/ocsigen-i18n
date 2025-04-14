# ocsigen-i18n

I18n made easy for OCaml apps.

The translations are described in tsv files.
A ppx extension is used to inject the text in the current language in your program.

Ocsigen-i18n supports simple strings to make it usable for any OCaml program.
It also has extensions for generating HTML elements with Tyxml or for client-server application programming with Eliom.

## Templating system

Define your translations into a tsv file. The first column is the key
to use (i.e. the name of the OCaml variable to use in your code).
The next columns are the translation for each language you want to
use.

```
foo	This is a simple key.	Ceci est une clé toute simple.
a_human a human un humain
bar	I am {{x}}.	Je suis {{x}}.
baz	There {{{c?are||is}}} apple{{{c?s||}}} here!	Il y a {{{c?des||une}}} pomme{{{c?s||}}} ici !
bu	I am {{x %s}} ({{n %d}}).	Je suis {{x %s}} ({{n %d}}).
```

- `{{x}}` defines a variable labeled `~x`
- `{{x %d}}` defines a variable labeled `~x`, and the `%d` format is used to
  display `x` as a string.
- `{{x?a||b}}` defines an optional boolean value `?x`.
  If `x` is `true` then `a` will be displayed.
  If it is `false` (which is the default), then `b` is chosen.

## Using the templates

To simplify the syntax, Ocsigen-i18n comes with a PPX extension.
For example `[%i18n foo]` will correspond to the text corresponding to key `foo` in the current language.
```ocaml
[%i18n foo]
[%i18n bar ~x:[%i18n a_human]]
[%i18n bar ~x:("Jean-Michel ("^string_of_int id^")")]
[%i18n baz]
[%i18n baz ~c:(nb > 1)]
[%i18n bu ~x:"Jean-Michel" ~n:id ]
```

If you are using Tyxml (and/or Eliom) (see below options `--tyxml` and `--eliom`),
the default will generate HTML elements:
```ocaml
[%i18n foo]
[%i18n bar ~x:[%i18n a_human]]
[%i18n bar ~x:[ txt "Jean-Michel ("
              ; txt @@ string_of_int id
              ; txt ")" ] ]
[%i18n baz]
[%i18n baz ~c:(nb > 1)]
[%i18n bu ~x:"Jean-Michel" ~n:id ]
```

The PPX will generate an HTML fragment as a list of Tyxml elements.
If you want a string
instead of a list of elements, prefix the variable name by `S.`, e.g.
`[%i18n S.bar ~s:[%i18n S.a_human]]`.

### Notes about the templates

A conditional value `{{{c?if_true||if_false}}}` will generate a function
taking an optional parameter `?(c=false)` to define if `if_true` or `if_false`
needs to be printed.

Languages does not need to use the same labeled variables. The compiler
will generate a function taking all the parameters it can detect when
parsing the template.

Variable name used twice refers to the same argument.


## ocsigen-i18n

Use command `ocsigen_i18n` to generate OCaml values for each text entry found in a tab-separated values file.
The TSV file contains a key in the first column (which will be used as OCaml function names),
and the corresponding text in different languages in other columns.

```
usage: ocsigen-i18n [options] [< input] [> output]
  --languages         Comma-separated languages (e.g. en,fr-fr, or Foo.Fr,Foo.Us if using external types). Must be ordered as in source TSV file.
  --default-language  Set the default language (if not specified, the first one in --languages will be the default).
  --input-file        TSV file containing keys and translations. If option is omitted or set to -, read on stdin.
  --output-file       File TSV file containing keys and translations. If option is omitted or set to -, write on stdout.
  --external-type     Values passed to --languages option come from a predefined type (do not generate the type nor from/to string functions).
  --primary           Generated file is secondary and depends on given primary file.
  --eliom             Generate code for a client-server Eliom app
  --tyxml             Generate code for a Tyxml-based app (for example a server-side Eliom app)
  --header            Generate only the file header 
  -help               Display this list of options
  --help              Display this list of options
```

Example (with the same tsv file as above):
```ocaml
$ ocsigen-i18n --languages en,fr < example.tsv 
type t = En|Fr
exception Unknown_language of string
let string_of_language = function 
  | En -> "en" | Fr -> "fr"
let language_of_string = function
  | "en" -> En | "fr" -> Fr| s -> raise (Unknown_language s)
let guess_language_of_string s = 
  try language_of_string s 
  with Unknown_language _ as e -> 
    try language_of_string (String.sub s 0 (String.index s '-')) 
    with Not_found -> 
      raise e 
let languages = [En;Fr]
let default_language = En
  (* We use a reference to store the lnaguage by default.
     Customize these functions if needed.
     For example use a scoped reference if you are using Eliom
     and want the language to depend on a session/tab or session group. *)
let _language_ = default_language
let get_language () = !_language_
let set_language language = _language_ := language

module Tr = struct
  let foo ?(lang = get_language ()) ()  () =
    match lang with
    | En -> "This is a simple key."
    | Fr -> "Ceci est une clé toute simple."
  let bar ?(lang = get_language ()) () ~x () =
    match lang with
    | En -> String.concat "" ["I am ";x;"."]
    | Fr -> String.concat "" ["Je suis ";x;"."]
  let baz ?(lang = get_language ()) () ?(c=false) () =
    match lang with
    | En -> String.concat "" ["There ";(if c then "are" else "is");" apple";(if c then "s" else "");" here!"]
    | Fr -> String.concat "" ["Il y a ";(if c then "des" else "une");" pomme";(if c then "s" else "");" ici !"]
  let bu ?(lang = get_language ()) () ~n ~x () =
    match lang with
    | En -> String.concat "" ["I am ";(Printf.sprintf "%s" x);" (";(Printf.sprintf "%d" n);")."]
    | Fr -> String.concat "" ["Je suis ";(Printf.sprintf "%s" x);" (";(Printf.sprintf "%d" n);")."]
end
```

### ocsigen-i18n-rewriter

`ocsigen-i18-rewriter` is the PPX extension that will generate automatically
the calls to these generated functions.

Just add option `ocsigen-i18n` in the list of PPX preprocessors in your Dune file,
and `--default_module DEFAULT` as option (where `DEFAULT` is the name of the module
generated by the `ocsigen-i18n` command).
Example: `(preprocess (pps ocsigen-i18n -- --default-module Example))` in your dune file.

Or, without Dune, use option `-ppx 'ocsigen-i18n-rewriter --default-module DEFAULT'`.

This will call the right function turning
`[%i18n foo]` into `DEFAULT.foo ()`,
`[%i18n S.bar ~x:"foo"]` into `DEFAULT.S.bar ~x:"foo" ()]` and so on...

#### Using several TSV files

You can use `ocsigen-i18n` to generate multiple `.ml` or `.eliom` files
from different TSV files. One of those will be the "default" one.

To specify a different module from the default one, you can do so using
expressions such as these:
 - `[%i18n MyI18n.foo]` will become `MyI18n.foo ()`
 - `[%i18n MyI18n.S.bar ~x:[%i18n S.foo]]` will become `MyI18n.S.bar ~x:(DEFAULT.S.foo ()) ()`

Because the rewriter infers your module from the module-path of your expression,
it needs to make the assumption that a module named `S` will *always* be
the one generated by `ocsigen-i18n` for strings.
**Do not name your i18n file `s.tsv`. It won't be compatible.**

#### Affixing the module names

Because the ppx already forces you to `i18n` in the expression,
and because most projects will want to keep sane files with `i18n` in their names,
it would be likely to end up with expressions like `[%i18n I18n_a.entry]`.

This is redundant and heavy. So, **if you like**, you can auto-prefix and auto-suffix your module names.

Calling the rewriter with `-ppx 'ocsigen-i18n-rewriter --prefix Pr_ --suffix _i18n --default-module DEFAULT'`
will prefix and suffix all **custom modules** (i.e. not the `DEFAULT` one) with `Pr_` and `_i18n`.

As such, `[%i18n Feature.foo]` will become `Pr_Feature_i18n.foo ()`,
allowing for clearer and more readable expressions.

The prefix *must* begin with a capital letter, to make it a legit module name start.

If you use these options, note that your file will need to account for the capital in the middle part.
`[%i18n Module.entry]` still needs `Module` to be a module name.

## Installation

```
opam install ocsigen-i18n
```
