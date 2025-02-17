module [
    is_two_character_token_start,
    is_digit,
    get_comment_length,
    get_number,
    get_string,
    is_ignored_character,
    is_identifier_start,
    get_identifier_or_keyword_string,
    get_identifier_or_keyword,
]

import Token exposing [Token]

is_two_character_token_start : U8 -> Bool
is_two_character_token_start = |char|
    char == '!' or char == '=' or char == '<' or char == '>' or char == '/'

is_digit : U8 -> Bool
is_digit = |c|
    List.range({ start: At('0'), end: At('9') })
    |> List.contains(c)

is_ignored_character : U8 -> Bool
is_ignored_character = |c|
    ignored_chars = ['\n', ' ', '\t']
    List.contains(ignored_chars, c)

is_alpha : U8 -> Bool
is_alpha = |c|
    uppercase_letters = List.range({ start: At('A'), end: At('Z') })
    lowercase_letters = List.range({ start: At('a'), end: At('z') })
    all_letters = List.join([uppercase_letters, lowercase_letters])

    List.contains(all_letters, c)

is_identifier_char : U8 -> Bool
is_identifier_char = |c|
    is_digit(c) or is_alpha(c) or (c == '_')

is_identifier_start : U8 -> Bool
is_identifier_start = |c|
    is_identifier_char(c) and !is_digit(c)

get_comment_length : List U8, U64 -> U64
get_comment_length = |chars, comment_start|
    count_chars_until_new_line = |count, char| if char == '\n' then Break(count) else Continue((count + 1))
    comment = List.drop_first(chars, comment_start)
    List.walk_until(comment, 0, count_chars_until_new_line)

# getString : List U8, U64 -> Result Str (Str.Utf8Problem Utf8ByteProblem U64)
get_string = |chars, string_start|
    accumulate_chars_until_quotation_mark = |cs, c|
        if c == '"' then
            Break(cs)
        else
            Continue(List.append(cs, c))

    string_contents_start = string_start + 1
    string_contents = List.walk_from_until(chars, string_contents_start, [], accumulate_chars_until_quotation_mark)

    length = List.len(string_contents) + 2
    string = try(Str.from_utf8, string_contents)

    Ok({ string, length })

get_number = |chars, number_start|
    accumulate_number_chars = |{ list, has_decimal }, c|
        when c is
            '.' if !has_decimal -> Continue({ list: List.append(list, c), has_decimal: Bool.true })
            n if is_digit(n) -> Continue({ list: List.append(list, n), has_decimal })
            _ -> Break({ list, has_decimal })

    number_chars_data =
        List.drop_first(chars, number_start)
        |> List.walk_until({ list: [], has_decimal: Bool.false }, accumulate_number_chars)

    number_chars = number_chars_data.list

    length = List.len(number_chars)

    number =
        number_chars_data.list
        |> try(Str.from_utf8)
        |> try(Str.to_f64)

    Ok({ number, length })

get_identifier_or_keyword_string = |chars, identifier_start|
    accumulate_identifier_chars = |cs, c|
        if is_identifier_char(c) then
            Continue(List.append(cs, c))
        else
            Break(cs)

    identifier_chars = List.walk_from_until(chars, identifier_start, [], accumulate_identifier_chars)
    length = List.len(identifier_chars)
    string = try Str.from_utf8(identifier_chars)

    Ok({ string, length })

get_identifier_or_keyword : { string : Str, length : U64 } -> { token : Token, length : U64 }
get_identifier_or_keyword = |{ string, length }|
    when string is
        "and" -> { token: And, length }
        "class" -> { token: Class, length }
        "else" -> { token: Else, length }
        "false" -> { token: False, length }
        "for" -> { token: For, length }
        "fun" -> { token: Fun, length }
        "nil" -> { token: Nil, length }
        "or" -> { token: Or, length }
        "print" -> { token: Print, length }
        "return" -> { token: Return, length }
        "super" -> { token: Super, length }
        "this" -> { token: This, length }
        "true" -> { token: True, length }
        "var" -> { token: Var, length }
        "while" -> { token: While, length }
        _ -> { token: Identifier string, length }

