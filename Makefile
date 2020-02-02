#******************************************************************************#
#                                                               _.._..,_,_     #
#    Makefile                                                  (          )    #
#                                                               ]~,'-.-~~[     #
#    By: mc <mc.maxcanal@gmail.com>                           .=])' (;  ([     #
#                                                             | ]:)   '  [     #
#      "THE BEER-WARE LICENSE" (Revision 42):                 | ]  :: '  [     #
#    As long as you retain this notice you can do whatever    '=]): .)  ([     #
#    you want with this stuff. If we meet some day, and you     |:: '   :|     #
#    think this is worth it, you can buy me a beer in return.    ~~----~~      #
#                                                                              #
#******************************************************************************#

##
## CUSTOM CONFIG
##

# name of the binary to make
PROJECT = swarm

# name of your ros script
ROS_SCRIPT = $(PROJECT).ros

# file-names of the sources
SRC_NAME = main.lisp  2d.lisp  boids.lisp  packages.lisp

# folder-names of the sources
VPATH = src  src/term  src/units

# where are your tests?
TEST_DIR = tests


##
## GLOBAL VARIABLES
##
BUILD_FLAGS =
# --remove-docstrings \
# --delete-debug-info \
# --destroy-packages-sbcl

# specify flags for commands used in the following rules
LN =		ln -s
RM =		rm -f
RMDIR =		rmdir
MKDIR =		mkdir -p
ROS ?=		ros
MAKE ?=		make -j$(shell nproc 2>/dev/null || echo 4)

# default to "pretty" Makefile, but you can run ´VERBOSE=t make´
# ifndef VERBOSE
#  ifndef CI
# .SILENT:
#  endif
# endif
PRINTF = test $(VERBOSE)$(CI) || printf

# some colors for pretty printing
WHITE =		\033[37m
RED =		\033[31m
GREEN =		\033[32m
YELLOW =	\033[33m
BLUE =		\033[34m
BASIC =		\033[0m


##
## PUBLIC RULES
##

# release build
all: deps
	+$(MAKE) $(PROJECT)

# install deps
deps:
	$(ROS) -e '(ql:quickload :swarm)'
# TODO: create symlink

# install dev deps
deps-test: $(PROJECT).asd
	$(ROS) -e '(ql:quickload :swarm/tests)'

# remove all junk files
clean:

# remove the generated binary, and all junk files
fclean: clean
	$(RM) $(PROJECT)

# some people like it real clean
mrproper: fclean

# clean build and recompile
re: fclean
	+$(MAKE) all

# run tests on project
test: deps-test
	$(ROS) -e '(asdf:test-system :swarm)'
	$(PRINTF) "All tests passed!\n"

# grep for all TODO tags in project
todo:
	! grep -rin todo . | grep -vE '^(Binary file|\./\.git|\./Makefile|flycheck_|\./\.build\.yml)'



##
## PRIVATE RULES
##

# build
$(PROJECT): $(SRC_NAME) $(ROS_SCRIPT)
	$(ROS) dump $(BUILD_FLAGS) executable $(ROS_SCRIPT) -o $@

# just to avoid conflicts between rules and files/folders names
.PHONY: all, $(PROJECT), \
deps, deps-test, \
clean, fclean, mrproper, re, test, todo
