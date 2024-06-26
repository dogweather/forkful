---
date: 2024-01-20 17:59:24.222044-07:00
description: 'How to: Alright, code time. Elm makes HTTP requests using the `Http`
  module. Here''s a quick example to fetch some JSON.'
lastmod: '2024-04-05T22:36:41.471495-06:00'
model: gpt-4-1106-preview
summary: Alright, code time. Elm makes HTTP requests using the `Http` module. Here's
  a quick example to fetch some JSON.
title: Sending an HTTP request
weight: 44
---

## How to:
Alright, code time. Elm makes HTTP requests using the `Http` module. Here's a quick example to fetch some JSON:

```Elm
import Http
import Json.Decode as Decode

type alias User =
    { id : Int
    , username : String
    }

userDecoder : Decode.Decoder User
userDecoder =
    Decode.map2 User
        (Decode.field "id" Decode.int)
        (Decode.field "username" Decode.string)

fetchUser : Cmd Msg
fetchUser =
    Http.get
        { url = "https://api.example.com/user/1"
        , decoder = userDecoder
        }
        |> Http.send UserFetched

type Msg
    = UserFetched (Result Http.Error User)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UserFetched (Ok user) ->
            ({ model | user = Just user }, Cmd.none)

        UserFetched (Err _) ->
            (model, Cmd.none)
```

Sample output when `UserFetched` is an `Ok user`:

```Elm
{ id = 1, username = "ElmerFudd" }
```

## Deep Dive
Sending HTTP requests isn't new; it's been the backbone of web communication since the 90s. Elm wraps up the complexity in the user-friendly `Http` module, focusing on safety and simplicity. Unlike the early days, Elm abstracts away the messy bits like XMLHttprequest and JSON parsing. Alternatives like using JavaScript's Fetch API or XMLHttpRequest directly are possible with ports, but Elm's built-in way keeps your code type-safe and pure. It handles side-effects through its powerful architecture without compromising your app's reliability.

## See Also
For more detailed explanations and troubleshooting, check out these resources:

- Elm package documentation for HTTP: [https://package.elm-lang.org/packages/elm/http/latest/](https://package.elm-lang.org/packages/elm/http/latest/)
- JSON Decoding in Elm: [https://package.elm-lang.org/packages/elm/json/latest/](https://package.elm-lang.org/packages/elm/json/latest/)
- Elm Guide on HTTP requests: [https://guide.elm-lang.org/effects/http.html](https://guide.elm-lang.org/effects/http.html)
- Elm Discuss for community insights: [https://discourse.elm-lang.org/](https://discourse.elm-lang.org/)
