app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.18.0/0APbwVN1_p1mJ96tXjaoiUCr8NBGamr8G8Ac_DrXR-o.tar.br" }

import pf.Stdout

add = |n| n + 3

will_you_mess_up = |{}| "lala"

main! = |_args|
    try(Stdout.line!, "${Inspect.to_str(add(2))}")
    try(Stdout.line!, will_you_mess_up({}))

    Ok({})
