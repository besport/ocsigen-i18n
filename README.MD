# ocsigen-i18n

I18n made easy for web sites written with
[eliom](https://github.com/ocsigen/eliom)

### ocsigen-i18n-generator

```
usage: ocsigen-i18n-generator [options] [< input] [> output]
  --langs         Comma-separated langs (from ocaml sum type) (e.g. Us,Fr). Must be ordered as in source TSV file.
  --default-lang  Set the default lang.
  --input-file    TSV file containing keys and translations. If option is omited or set to -, read on stdin.
  --ouput-file    File TSV file containing keys and translations. If option is omited or set to -, write on stdout.
```

### ocsigen-i18n-rewriter

Use the `-ppx 'ocsigen-i18n-rewriter MODULE_NAME'` where `MODULE_NAME`
is the name of the module generated with `ocsigen-i18n-generator` (basename
of the output file)

This will call the right function turning
`[%i18n foo]` into `MODULE_NAME.foo ()`,
`[%i18n S.bar ~x:"foo"]` into `MODULE_NAME.S.bar ~x:"foo" ()] and so on...

## Installation

```
opam pin add ocsigen-i18n https://github.com/besport/ocsigen-i18n.git
opem install ocsigen-i18n
```

### How to

#### Templating system

Define your translations into a tsv file. The first column is the key
to use (i.e. the name of the ocaml variable to use in your code).
The following columns are the the translation for each language you want to
use.


```
foo	This is a simple key.	Ceci est une clé toute simple.
bar	I am {{x}}.	Je suis {{x}}.
baz	There {{{c?are||is}}} apple{{{c?s||}}} here!	Il y a {{{c?des||une}}} pomme{{{c?s||}}} ici !
```

#### Using the templates

```ocaml
[%i18n foo]
[%i18n bar ~x:[%i18n a_human]]
[%i18n baz]
[%i18n baz ~c:(nb > 1)]
```

This will generate an HTML fragment (a list of elements). If you are interested
into strings instead of element list, prefix the variable name by `S.`, e.g.
`[%i18n S.bar ~s:[%i18n S.a_human]]`.

#### Notes about the templates

A conditional value `{{{c?if_true||if_false}}}` will generate a function
taking an optional parameter `?(c=false)` to define if `if_true` or `if_false`
needs to be printed.

Languages does not need to use the same labelled variables. The compiler
will generate a function taking all the parameters it can detect when
parsing template.

Variable name used twice refers the same argument.
