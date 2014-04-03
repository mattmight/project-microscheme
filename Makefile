
.PHONY: default
default: bin/microscheme

bin:
	mkdir bin

bin/microscheme: bin src/microscheme.rkt
	raco exe -o bin/microscheme src/microscheme.rkt 


expected:
	mkdir expected
	for s in tests/*.ms; do echo "#lang racket\n(let () (begin " > tmp; cat $$s >> tmp; echo ')) (newline)' >> tmp; racket tmp > expected/`basename $$s`.out;  done
	rm tmp

results: bin/microscheme
	rm -rf results
	mkdir results
	for s in tests/*.ms; do bin/microscheme < $$s > results/`basename $$s`.out; done

test.txt: expected results
	for s in expected/*.ms.out; do if s-diff $$s ./results/`basename $$s`; then echo `basename $$s` passed; else echo `basename $$s` failed; fi done > test.txt
	
.PHONY: test
test: test.txt
	cat test.txt

clean:
	rm -rfv bin expected results test.txt
