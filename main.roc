app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.Stdout
import pf.Arg exposing [Arg]
import pf.File
import Scanner exposing [scan]

parse = |source|
    tokens = try scan(source)
    Ok(Inspect.to_str(tokens))

# main! : List Arg => Result {} [Exit I32 Str]
main! = |args|
    run!(args)

run! = |args|
    # rec = { snakes: 2, birds: 3 }
    # { snakes, birds } = rec

    try Stdout.line!("Executing file:")

    file =
        List.map(args, Arg.display)
        |> List.get(1)
        |> try

    file_contents = try(File.read_utf8!, file)

    processed_file = try parse(file_contents)

    try(Stdout.line!, "${processed_file}")

    Ok({})
