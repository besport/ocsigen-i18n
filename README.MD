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

## Installation

```
opam pin add ocsigen-i18n https://github.com/besport/ocsigen-i18n.git
opem install ocsigen-i18n
```
