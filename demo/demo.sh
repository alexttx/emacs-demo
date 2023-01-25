#!/bin/bash

CMD=${0##*/}

more=1
fake=0
load_args=()
title=()

while (( more && $# > 0 )); do
    case "$1" in

        (-n)
            fake=1
            shift
            ;;

        (-h)
            echo "Usage: $CMD [Options] [FEATURE.el] [EMACS_ARGS]"
            echo "A wrapper to run emacs with different features enabled."
            echo "Options:"
            echo "  -h  # show this help"
            echo "  -n  # show emacs command line instead of running emacs"
            echo "Examples:"
            echo "    $CMD ~/project/main.c             # run w/ no demo features"
            echo "    $CMD *.el ~/project/main.c        # run w/ all demo features"
            echo "    $CMD orderless.el vertico.el ~/project/main.c # run w/ some features"
            exit 0
            ;;

        (demo.el)
            shift
            ;;

        (*.el)
            title+=(${1%.el})
            load_args+=(-l "$1")
            shift
            ;;

        (*)
            more=0
            ;;
    esac
done

if (( fake )); then
    echo emacs -q -g 80x34 "${load_args[@]}" -l demo.el --name "${title[*]}" "$@"
else
    emacs -q -g 80x34 "${load_args[@]}" -l demo.el --name "${title[*]}" "$@"
fi
