
CMD=${0##*/}
CMD_DIR=$(readlink -m "${BASH_SOURCE[0]}"/..) || { echo "readlink failed..." 1>&2; exit 1; }

show () {
    features=()
    while (( $# > 0 )); do
        if [[ "$1" == "--" ]]; then
            shift
            break;
        fi
        features+=("$1")
        shift
    done

    for f in "${features[@]}"; do
        echo "$CMD_DIR/$CMD $f -- $@ & disown; sleep 2"
    done
    echo
}
    

if [[ $# == 1 && "$1" == "-h" ]]; then

    show "" -- lib/cn
    show "" "orderless" "vertico" "orderless vertico" -- lib/cn
    show "" "consult" "consult vertico" "consult vertico orderless" -- docs/cn_omf.md 
    show "" "gtags" "gtags gxref" "gtags gxref orderless vertico" -- +28:11 lib/cn/blk_list.c
    show "projectile" "projectile orderless vertico" -- lib/cn

    show "gtags gxref orderless vertico" \
         "lsp-mode orderless vertico" \
         "lsp-mode gtags gxref orderless vertico" \
         -- +28:11 lib/cn/blk_list.c

    # lsp-ui
    # lsp-ivy
    # lsp-treemacs
    exit 0
fi

features=()
while (( $# > 0 )); do
    if [[ "$1" == "--" ]]; then
        shift
        break;
    fi
    features+=("$1")
    shift
done

settings=()
settings+=(my-use-which-key t)
for f in "${features[@]}"; do
    name=${features[*]}
    settings+=("my-use-$f" t)
done

cd ~/w/src/hse/hse
emacs -q -g 80x34 -eval "(setq ${settings[*]})" -l "$CMD_DIR"/init.el --name "demo: ${features[*]}" "$@"

