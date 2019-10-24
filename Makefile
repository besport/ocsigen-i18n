OCAMLOPT=ocamlfind ocamlopt
CHECKER=ocsigen-i18n-checker
REWRITER=ocsigen-i18n-rewriter
GENERATOR=ocsigen-i18n-generator

PROGS=${GENERATOR} ${REWRITER} ${CHECKER}

build: ${PROGS}

${GENERATOR}: i18n_generate.mll
	ocamllex i18n_generate.mll
	${OCAMLOPT} -package str -linkpkg -o $@ i18n_generate.ml

$(CHECKER): i18n_ppx_common.ml i18n_ppx_checker.ml
	${OCAMLOPT} -package str -package compiler-libs.common -linkpkg -o $@ $^

${REWRITER}: i18n_ppx_common.ml i18n_ppx_rewriter.ml
	${OCAMLOPT} -package str,compiler-libs.common,ppx_tools.metaquot -linkpkg -o $@ $^

clean:
	-rm -f *.cmi *.cmx *.o *~ *#
	-rm -f i18n_generate.ml
	-rm -f ${GENERATOR} ${REWRITER} ${CHECKER}

install: ${PROGS}
ifndef bindir
	${error bindir is not set}
else
	cp ${PROGS} ${bindir}
endif

uninstall:
ifndef bindir
	${error bindir is not set}
else
	$(RM) ${bindir}/${GENERATOR}
	$(RM) ${bindir}/${REWRITER}
	$(RM) ${bindir}/${CHECKER}
endif
