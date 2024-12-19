#!/bin/bash

CMD_DIR=$(readlink -m "${BASH_SOURCE[0]}"/..) || { echo "readlink failed..." 1>&2; exit 1; }

DEMO_PACKAGES=$(ls "$CMD_DIR"/*.el | sed -e 's,.*/,,' -e 's,\.el$,,' | sort)

err () {
    while (( $# > 0 )); do
        echo "$1"
        shift
    done 1>&2
    exit 1
}

syntax () {
    err "$@" "Use -h for help"
}

help ()
{
    cat<<EOF
Usage: $CMD [options] [<packages>] [--] [<extra>]
Options:
  -h  // show help
  -n  // show emacs command but don't run it
  -v  // show emacs command before running it

Notes:
  - Packages can be listed by name (e.g., "which-key") or by
    file (e.g., "which-key.el").
  - Extra args are passed to Emacs.

Available packages:
EOF
    for p in $DEMO_PACKAGES; do
        echo "  $p"
    done
    exit 0
}

is_demo_file () {
    local f=$1
    local p
    shift
    for p in $DEMO_PACKAGES; do
        if [[ "$f" == "$p" ]]; then
            echo "$CMD_DIR/$f.el"
            return 0
        fi
    done
    if [[ -f "$f" ]]; then
        echo "$f"
        return 0
    fi
    if [[ -f "$f".el ]]; then
        echo "$f".el
        return 0
    fi
    return 1
}
             
load_args=()
load_names=()
fake=0
verbose=0
more=1
while (( more && $# > 0 )); do
    case "$1" in
        (-h) help;;
        (-v) verbose=1; shift;;
        (-n) verbose=1; fake=1; shift;;
        (--) more=0; shift;;
        (-*) syntax "Invalid option: $1";;
        (*)  f=$(is_demo_file "$1") || err "Cannot demo init file for '$1'"
             load_args+=(-l "$f")
             name=$(basename "$f")
             name=${name%.el}
             load_names+=($name)
             shift
             ;;
    esac
done

if (( ${#load_names[@]} == 0 )); then
    name="none"
else
    name="${load_names[*]}"
fi

emacs_args=(-Q "${load_args[@]}" -l "$CMD_DIR"/../demo-init.el --name "$name" "$@")
if (( verbose || fake )); then
    echo emacs "${emacs_args[@]}"
fi
if (( ! fake )); then
    emacs "${emacs_args[@]}"
fi                                                 
