BUILDDIR=_build
VPATH=$(BUILDDIR)
OCAMLDIR=$(shell ocamlopt -where)
$(shell mkdir -p $(BUILDDIR) $(BUILDDIR)/stub $(BUILDDIR)/lib $(BUILDDIR)/stub_generator $(BUILDDIR)/test $(BUILDDIR)/generated)
PACKAGES=bap,bap.plugins,ctypes.stubs,ctypes.foreign,findlib.dynload

BINDINGS=$(BUILDDIR)/lib/cident.cmi			\
	 $(BUILDDIR)/lib/cident.cmx			\
	 $(BUILDDIR)/lib/ctype.cmi			\
	 $(BUILDDIR)/lib/ctype.cmx			\
	 $(BUILDDIR)/lib/cenum.cmi			\
	 $(BUILDDIR)/lib/cenum.cmx			\
	 $(BUILDDIR)/lib/cstring.cmi			\
	 $(BUILDDIR)/lib/cstring.cmx			\
	 $(BUILDDIR)/lib/opaque.cmi			\
	 $(BUILDDIR)/lib/opaque.cmx			\
	 $(BUILDDIR)/lib/cmodule.cmi			\
	 $(BUILDDIR)/lib/cmodule.cmx			\
	 $(BUILDDIR)/lib/bap_internal.cmi		\
	 $(BUILDDIR)/lib/bap_internal.cmx		\
	 $(BUILDDIR)/lib/bindings.cmi			\
	 $(BUILDDIR)/lib/bindings.cmx			\
# The files used to build the stub generator.
GENERATOR_FILES=$(BINDINGS)				\
                $(BUILDDIR)/stub_generator/generate.cmx

# The files from which we'll build a shared library.
LIBFILES=$(BINDINGS)					\
         $(BUILDDIR)/generated/bap_bindings.cmx	 	\
         $(BUILDDIR)/lib/apply_bindings.cmx		\
         $(BUILDDIR)/generated/bap.o

SOURCEFILES=$(wildcard lib/*.mli stub_generator/*.mli lib/*.ml stub_generator/*.ml)

CAML_INIT=$(BUILDDIR)/stub/init.o

# The files that we'll generate
GENERATED=$(BUILDDIR)/generated/bap.h \
          $(BUILDDIR)/generated/bap.c \
          $(BUILDDIR)/generated/bap_bindings.ml

GENERATOR=$(BUILDDIR)/generate$(EXTEXE)

OSTYPE:=$(shell ocamlfind ocamlc -config | awk '/^os_type:/ {print $$2}')
SYSTEM:=$(shell ocamlfind ocamlc -config | awk '/^system:/ {print $$2}')
EXTDLL:=$(shell ocamlfind ocamlc -config | awk '/^ext_dll:/ {print $$2}')
CC:= $(shell ocamlfind ocamlc -config | awk '/^bytecomp_c_compiler/ {for(i=2;i<=NF;i++) printf "%s " ,$$i}')

ifeq ($(OSTYPE),$(filter $(OSTYPE),Win32 Cygwin))
EXTEXE=.exe
else
EXTEXE=
endif


all: stubs sharedlib

sharedlib: $(BUILDDIR)/libbap$(EXTDLL)


ifeq ($(OSTYPE),$(filter $(OSTYPE),Win32 Cygwin))
$(BUILDDIR)/libbap$(EXTDLL): $(CAML_INIT) $(LIBFILES)
	ocamlfind opt -o $@ -linkpkg -output-obj -verbose -package $(PACKAGES) $(filter %.cmx,$(LIBFILES))
else ifeq ($(SYSTEM),$(filter $(SYSTEM),macosx))
$(BUILDDIR)/libbap$(EXTDLL): $(CAML_INIT) $(LIBFILES)
	ocamlfind opt -o $@ -linkpkg -runtime-variant _pic -verbose -ccopt -dynamiclib -package $(PACKAGES) $(filter %.cmx,$(LIBFILES))
else
$(BUILDDIR)/libbap$(EXTDLL): $(CAML_INIT) $(LIBFILES)
	ocamlfind opt -o $@ -linkpkg -output-obj -runtime-variant _pic -verbose -package $(PACKAGES) $(filter %.cmx,$(LIBFILES))
endif

stubs: $(GENERATED)

$(BUILDDIR)/stub/%.o:
	ocamlc -g -c stub/init.c
	mv init.o $@

$(GENERATED): $(GENERATOR)
	$(GENERATOR) $(BUILDDIR)/generated

$(BUILDDIR)/%.o: %.c
	$(CC) -c -o $@ -fPIC -I $(shell ocamlfind query ctypes) -I $(OCAMLDIR) -I $(OCAMLDIR)/../ctypes $<

$(BUILDDIR)/%.cmx: %.ml
	ocamlfind opt -c -o $@ -I $(BUILDDIR)/generated -I $(BUILDDIR)/lib -package $(PACKAGES) $<

$(BUILDDIR)/%.cmi: %.mli
	ocamlfind opt -c -o $@ -I $(BUILDDIR)/generated -I $(BUILDDIR)/lib -package $(PACKAGES) $<

$(GENERATOR): $(GENERATOR_FILES)
	ocamlfind opt -o $@ -linkpkg -package $(PACKAGES) $(filter %.cmx,$(GENERATOR_FILES))

clean:
	rm -rf $(BUILDDIR)


.PHONY: test
test: all
	$(MAKE) -C $@
ifeq ($(OSTYPE),Win32)
	PATH="$(BUILDDIR):$(PATH)" _build/test/test.native
else
	LD_LIBRARY_PATH=$(BUILDDIR) _build/test/test.native
endif


depend: $(SOURCEFILES)
	ocamlfind ocamldep -I lib $^ > .depend


include .depend
