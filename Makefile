CCL=ccl

test:
	make clean
	$(CCL) --no-init --batch -l load.lisp -e '(let ((f (run-tests))) (when (find "FORMAT.B.9" f :key (function symbol-name) :test (function string=)) (load "tests:extended-tests;ki317-probes.lisp") (format t "~&KI317 RESULT b9=FAIL ~s~%" (funcall (find-symbol "KI317-PROBES" "CL-USER"))) (finish-output)) (ccl:quit (if f 1 0)))'

test-ccl:
	make clean
	$(CCL) --no-init --batch -l load.lisp -e "(run-tests :ansi nil :exit t)"

test-stress:
	make clean
	$(CCL) --no-init --batch -l load.lisp -e "(run-tests :ansi nil :ccl nil :exit t)"

# The extended tests are NOT part of `test': several of them reproduce defects
# that are not fixed yet and are expected to fail until the fix lands.  See
# extended-tests/README.md.
test-extended:
	bash extended-tests/run-all.sh $(CCL)

clean:
	(cd ansi-tests && make clean)
