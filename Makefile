# Define the necessary paths
SRC_DIR := ./src
BIN_DIR := ./bin
DOC_DIR := ./doc

# The OCaml libraries required to build the binaries
LIBS =

# The target binary
TARGET := $(BIN_DIR)/play

# The source modules required to build the target
MODS := countdown CLI test play

# Compiler settings; use the native-code compiler by default
DEBUG ?= false
ifeq ($(DEBUG), false)
    OCC = ocamlopt
else
    OCC = ocamlc -g
endif
OCC_FLAGS = -thread -linkpkg -I $(SRC_DIR)

#
# The remaining variables are derived from the preceding definitions
#

# Module source and object files
MODS := $(MODS:%=$(SRC_DIR)/%)
MODS_ML := $(MODS:%=%.ml)
MODS_MLI := $(MODS:%=%.mli)
MODS_SRC := $(MODS_MLI) $(MODS_ML)
OBJS := $(MODS:%=%.cmo) $(MODS:%=%.cmx) $(MODS:%=%.cmi) $(MODS:%=%.o)

# OCamldoc settings
ODOC_CSS = share/ocamldoc/style.css
ODOC_INTRO = share/ocamldoc/intro.mldoc
ODOC_TITLE = Countdown
ODOC_IDX = $(DOC_DIR)/index.html
ODOC_FLAGS = -html -sort -colorize-code -d $(DOC_DIR) -I $(SRC_DIR)
ODOC_FLAGS += -intro $(ODOC_INTRO) -t "$(ODOC_TITLE)"

#
# Makefile rules; generate all binaries and the API documentation by default
#

all: $(TARGET) $(ODOC_IDX)

$(TARGET) $(OBJS): $(MODS_SRC) | $(BIN_DIR)
	@ocamlfind $(OCC) -package "$(LIBS)" $(OCC_FLAGS) -o $@ \
		unix.cmxa $(MODS_SRC)

$(BIN_DIR):
	@mkdir $(BIN_DIR)

$(ODOC_IDX): $(MODS_MLI) $(OBJS) $(ODOC_CSS) $(ODOC_INTRO) | $(DOC_DIR)
	@ocamlfind ocamldoc $(ODOC_FLAGS) $(MODS_MLI)
	@cp $(ODOC_CSS) $(DOC_DIR)/style.css

$(DOC_DIR):
	@mkdir $(DOC_DIR)

clean:
	@rm -f $(OBJS)

distclean: clean
	@rm -rf $(DOC_DIR) $(TARGET) $(BIN_DIR)

.PHONY: all docs clean distclean
