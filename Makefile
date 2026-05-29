GHC = ghc
GHC_FLAGS = -Wall -Wno-unused-imports -Wno-name-shadowing -Wno-x-partial

# Send all .o / .hi intermediates into BUILD_DIR so the source tree
# stays clean. `make clean` just rm -rf's the directory.
BUILD_DIR = build
GHC_OUT   = -odir $(BUILD_DIR) -hidir $(BUILD_DIR)

SRCS      = Lambda.hs Code.hs Parser.hs Default.hs DeBruijn.hs
TEST_SRCS = $(SRCS) Tests/Grader.hs Tests/LambdaTest.hs Tests/ParserTest.hs \
            Tests/CodeTest.hs Tests/DeBruijnTest.hs Tests/Main.hs

.PHONY: all repl test clean

all: repl

repl: $(SRCS) main.hs
	$(GHC) $(GHC_FLAGS) $(GHC_OUT) -o repl main.hs

test: $(TEST_SRCS)
	$(GHC) $(GHC_FLAGS) $(GHC_OUT) -o run_tests Tests/Main.hs
	./run_tests

clean:
	rm -rf $(BUILD_DIR) repl run_tests
