OCAMLPREFIX=
SRC=src/*
all:converter
build/src/analyzer.native: $(SRC)
	ocamlbuild analyzer.native -I src -build-dir build
converter: build/src/analyzer.native
	cp $^ $@
clean:
	rm -rf build/
demo: demo.java converter
	./converter demo.java
test: demo.java converter
	@mkdir -p build
	./converter demo.java > build/demo.smt2
	@if [ $$(which z3) = "" ]; then echo "Failed to find z3 solver"; fi
	@which z3  > /dev/null
	@echo "Using z3 solver : $$(which z3). And launching computation..."
	@echo "Result is : " $$(z3 build/demo.smt2)
	@echo "Expected is : sat (or unknown or timeout)"
	
.PHONY: demo test clean
