.PHONY: test test-verbose watch

test:
	@find _build/default -name 'test_*.exe' -exec {} \;

test-verbose:
	@find _build/default -name 'test_*.exe' -print0 | \
		xargs -0 -I{} sh -c 'echo "\n=== {} ===" && {} -verbose true'

watch:
	dune build @all -w
