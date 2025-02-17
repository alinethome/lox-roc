module [scan]

import pf.Stdout
import Token exposing [Token]
import ScannerHelper exposing [
    is_two_character_token_start,
    is_digit,
    is_identifier_start,
    get_comment_length,
    get_number,
    get_string,
    is_ignored_character,
    get_identifier_or_keyword_string,
    get_identifier_or_keyword,
]

scan : Str -> Result (List Token) _
scan = |source|
    chars = Str.to_utf8(source)
    scan_helper(Ok([]), chars, 0)

scan_helper : Result (List Token) _, List U8, U64 -> Result (List Token) _
scan_helper = |maybe_tokens, chars, index|
    tokens = try(maybe_tokens)

    when List.get(chars, index) is
        Ok(c) ->
            { token, index_next } = try(scan_token, c, chars, index)

            new_tokens =
                when token is
                    IgnoredChars -> tokens
                    _ -> List.append(tokens, token)

            scan_helper(Ok(new_tokens), chars, index_next)

        Err(OutOfBounds) -> Ok(List.append(tokens, EndOfFile))

scan_token : U8, List U8, U64 -> Result { token : Token, index_next : U64 } _
scan_token = |char, chars, index|
    when char is
        '(' -> Ok({ token: LeftParen, index_next: index + 1 })
        ')' -> Ok({ token: RightParen, index_next: index + 1 })
        '{' -> Ok({ token: LeftBrace, index_next: index + 1 })
        '}' -> Ok({ token: RightBrace, index_next: index + 1 })
        ',' -> Ok({ token: Comma, index_next: index + 1 })
        '.' -> Ok({ token: Dot, index_next: index + 1 })
        '-' -> Ok({ token: Minus, index_next: index + 1 })
        '+' -> Ok({ token: Plus, index_next: index + 1 })
        ';' -> Ok({ token: Semicolon, index_next: index + 1 })
        '*' -> Ok({ token: Star, index_next: index + 1 })
        '"' ->
            { string, length } = try(get_string(chars, index))
            Ok({ token: String(string), index_next: index + length })

        c if is_two_character_token_start(c) ->
            { token, size } = disambiguate_one_and_two_character_tokens(c, chars, (index + 1))
            Ok({ token, index_next: index + size })

        c if is_digit(c) ->
            { number, length } = try get_number(chars, index)
            Ok({ token: Number(number), index_next: index + length })

        c if is_identifier_start(c) ->
            string_with_length = try get_identifier_or_keyword_string(chars, index)
            { token, length } = get_identifier_or_keyword(string_with_length)
            Ok({ token, index_next: index + length })

        c if is_ignored_character(c) -> Ok({ token: IgnoredChars, index_next: index + 1 })
        c -> Err(UnknownChar(c))

disambiguate_one_and_two_character_tokens : U8, List U8, U64 -> { token : Token, size : U64 }
disambiguate_one_and_two_character_tokens = |char, chars, index_next|
    when char is
        '!' ->
            when List.get(chars, index_next) is
                Ok('=') -> { token: BangEqual, size: 2 }
                _ -> { token: Bang, size: 1 }

        '=' ->
            when List.get(chars, index_next) is
                Ok('=') -> { token: EqualEqual, size: 2 }
                _ -> { token: Equal, size: 1 }

        '<' ->
            when List.get(chars, index_next) is
                Ok('=') -> { token: LessEqual, size: 2 }
                _ -> { token: Less, size: 1 }

        '>' ->
            when List.get(chars, index_next) is
                Ok('=') -> { token: GreaterEqual, size: 2 }
                _ -> { token: Greater, size: 1 }

        '/' ->
            when List.get(chars, index_next) is
                Ok('/') ->
                    comment_start = index_next - 1
                    size = get_comment_length(chars, comment_start)
                    { token: IgnoredChars, size }

                _ ->
                    { token: Slash, size: 1 }

        _ -> crash("Unreachable code")

