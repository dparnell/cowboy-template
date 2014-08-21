PROJECT = template
DEPS = cowboy erlydtl

dep_cowboy = pkg://cowboy 1.0.0
dep_erlydtl = git://github.com/erlydtl/erlydtl.git 0.7.0

include erlang.mk
