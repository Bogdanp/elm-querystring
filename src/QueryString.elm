module QueryString
    exposing
        ( QueryString
        , parse
        , empty
        , render
        , add
        , addList
        , addMaybe
        , remove
        , filter
        , all
        , one
        , many
        , string
        , int
        )

{-| This module exposes functions for working with query strings.

You can manipulate `QueryString`s:

    > empty
    |   |> add "a" "hello"
    |   |> add "a" "goodbye"
    |   |> add "b" "1"
    |   |> render
    "?a=hello&a=goodbye&b=1" : String

And you can parse and extract their parameters:

    > let
    |   qs = parse "?a=1&a=2&a=test&b=hello"
    |   a = qs |> many int "a"
    |   b = qs |> one string "b" |> Maybe.withDefault "goodbye"
    | in
    |   (a, b)
    ([1, 2], "hello")

## Types
@docs QueryString

## Constructing QueryStrings
@docs parse, empty

## Manipulating parameters
@docs render, add, addList, addMaybe, remove, filter

## Extracting parameters
@docs all, one, many

### Parsers
@docs string, int

-}

import Combine exposing (..)
import Combine.Num
import Dict exposing (Dict)
import Http exposing (decodeUri, encodeUri)
import String


{-| Represents a parsed query string.
-}
type QueryString
    = QueryString (Dict String (List String))


{-| Construct an empty QueryString.
-}
empty : QueryString
empty =
    QueryString Dict.empty


{-| Turn a String into a QueryString. The initial `?` is optional.

    > parse ""
    QueryString (Dict.fromList []) : QueryString

    > parse "?a=1&b=c&a=2"
    QueryString (Dict.fromList [("a",["1","2"]),("b",["c"])])
        : QueryString

    > parse "a=1&b=c&a=2"
    QueryString (Dict.fromList [("a",["1","2"]),("b",["c"])])
        : QueryString

-}
parse : String -> QueryString
parse =
    Combine.parse query
        >> Result.toMaybe
        >> Maybe.map (\( _, _, d ) -> QueryString d)
        >> Maybe.withDefault empty


{-| Retrieve all of the values for a given key.

    > parse "?a=1&a=2"
    |   |> all "a"
    ["1","2"] : List String

    > parse "?a=1&a=2"
    |   |> all "b"
    [] : List String

-}
all : String -> QueryString -> List String
all k (QueryString qs) =
    Dict.get k qs
        |> Maybe.withDefault []


{-| Retrieve a single value for a given key. Values are funneled through
the given parser before being returned.

    > parse "?a=1&a=2"
    |   |> one string "a"
    Just "2" : Maybe.Maybe String

    > parse "?a=1&a=2"
    |   |> one int "a"
    Just 2 : Maybe.Maybe Int

    > parse "?a=1&a=c"
    |   |> one int "a"
    Just 1 : Maybe.Maybe Int

-}
one : Parser () a -> String -> QueryString -> Maybe a
one p k =
    many p k >> List.head


{-| Retrieve zero or more values for some key. Values are funneled
through the given parser before being returned.

    > parse "?a=1&a=c&a=2"
    |   |> many int "a"
    [1,2] : List Int

-}
many : Parser () a -> String -> QueryString -> List a
many p k =
    all k >> List.filterMap (maybeParse p)


{-| A Parser that accepts any string.
-}
string : Parser s String
string =
    regex ".*"


{-| A Parser that accepts any integer.
-}
int : Parser s Int
int =
    Combine.Num.int


{-| Render a QueryString to a String.

    > render (parse "?a=1&b=a&a=c")
    "?a=1&a=c&b=a" : String

-}
render : QueryString -> String
render (QueryString qs) =
    let
        flatten ( k, xs ) =
            List.map (\x -> k ++ "=" ++ encodeUri x) xs
    in
        Dict.toList qs
            |> List.concatMap flatten
            |> String.join "&"
            |> (++) "?"


{-| Add a value to a key.

    > parse "?a=1&b=a&a=c"
    |   |> add "a" "2"
    |   |> render
    "?a=2&a=1&a=c&b=a" : String

    > parse "?a=1&b=a&a=c"
    |   |> add "d" "hello"
    |   |> render
    "?a=1&a=c&b=a&d=hello" : String

-}
add : String -> String -> QueryString -> QueryString
add k v (QueryString qs) =
    let
        prepend xs =
            case xs of
                Nothing ->
                    Just [ v ]

                Just xs ->
                    Just (v :: xs)
    in
        Dict.update k prepend qs
            |> QueryString


{-| Add a list of values to a key.

    > parse "?a=1&b=a&a=c"
    |   |> addList "a" [ "2", "3" ]
    |   |> render
    "?a=3&a=2&a=1&a=c&b=a" : String

    > parse "?a=1&b=a&a=c"
    |   |> addList "d" [ "hello", "world" ]
    |   |> render
    "?a=1&a=c&b=a&d=world&d=hello" : String

-}
addList : String -> List String -> QueryString -> QueryString
addList k l qs =
    case l of
        [] ->
            qs

        v :: vs ->
            add k v qs |> addList k vs


{-| Add a `Maybe` to a key.

If the value is `Nothing`, don't add anything. If the value is `Just
v`, add `v`.

    > parse "?a=1&b=a&a=c"
    |   |> addMaybe "a" Nothing
    |   |> render
    "?a=1&a=c&b=a" : String

    > parse "?a=1&b=a&a=c"
    |   |> addMaybe "d" (Just "hello")
    |   |> render
    "?a=1&a=c&b=a&d=hello" : String

-}
addMaybe : String -> Maybe String -> QueryString -> QueryString
addMaybe k m qs =
    case m of
        Nothing ->
            qs

        Just v ->
            add k v qs


{-| Remove a key.

    > parse "?a=1&b=a&a=c"
    |   |> remove "a"
    |   |> render
    "?b=a" : String

    > parse "?a=1&b=a&a=c"
    |   |> remove "c"
    |   |> render
    "?a=1&a=c&b=a" : String

-}
remove : String -> QueryString -> QueryString
remove k (QueryString qs) =
    Dict.remove k qs
        |> QueryString


{-| Filter a key's values.

    > parse "?a=1&b=a&a=c"
    |   |> filter "a" ((==) "1")
    |   |> render
    "?a=1&b=a" : String

-}
filter : String -> (String -> Bool) -> QueryString -> QueryString
filter k f (QueryString qs) =
    let
        remove xs =
            Maybe.map (List.filter f) xs
    in
        Dict.update k remove qs
            |> QueryString


parameter : Parser s ( String, String )
parameter =
    let
        key =
            regex "[^=]+"

        value =
            regex "[^&]*"

        param k v =
            ( k, decodeUri v |> Maybe.withDefault "" )
    in
        param <$> (key <* Combine.string "=") <*> value


parameters : Parser s (List ( String, String ))
parameters =
    sepBy (Combine.string "&") parameter <* (skip (Combine.string "#") <|> end)


query : Parser s (Dict String (List String))
query =
    let
        prepend y xs =
            case xs of
                Nothing ->
                    Just [ y ]

                Just xs ->
                    Just (y :: xs)

        collect ( k, x ) d =
            Dict.update k (prepend x) d
    in
        List.foldr collect Dict.empty <$> (maybe (Combine.string "?") *> parameters)


maybeParse : Parser () a -> String -> Maybe a
maybeParse p =
    Combine.parse p
        >> Result.toMaybe
        >> Maybe.map (\( _, _, x ) -> x)
