#!/bin/bash

CMD=${0##*/}
CMD_DIR=$(readlink -m "${BASH_SOURCE[0]}"/..) || { echo "readlink failed..." 1>&2; exit 1; }
DEMO_ELISP=$CMD_DIR/enable

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
            echo "Usage: $CMD [options] [enable/<feature>.el enable/<feature>.el ...] [--] [<emacs_args>]"
            echo ""
            echo "$CMD is a wrapper that runs emacs in a sandbox using initialization files"
            echo "from this tutorial's 'emacs.d' directory instead of the user's .emacs"
            echo "file or .emacs.d/ directory."
            echo ""
            echo "Options:"
            echo "  -h  # show this help"
            echo "  -n  # show emacs command but do not run emacs"
            echo ""
            echo "Examples:"
            echo ""
            echo "  # edit a file none of this demo's features enabled:"
            echo "  $CMD ~/project/main.c"
            echo ""
            echo "  # edit a file with 'orderless' and 'vertico' enabled:"
            echo "  $CMD enable/orderless.el enable/vertico.el ~/project/main.c"
            echo ""
            echo "  # Edit a file with all of this demo's features enabled:"
            echo "  $CMD enable/*.el ~/project/main.c"
            exit 0
            ;;

        (*demo-init.el)
            shift
            ;;

        (*.el)
            file=$1
            shift
            name=$(basename "$1")
            title+=(${name%.el})
            load_args+=(-l "$1")
            shift
            ;;
        (--)
            shift
            more=0
            ;;
        (*)
            more=0
            ;;
    esac
done


if (( fake )); then
    echo emacs -q -g 80x34 "${load_args[@]}" -l enable/demo-init.el --name "${title[*]}" "$@"
else
    emacs -q -g 80x34 "${load_args[@]}" -l enable/demo-init.el --name "${title[*]}" "$@"
fi
