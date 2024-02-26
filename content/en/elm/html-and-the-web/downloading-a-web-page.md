---
date: 2024-01-20 17:43:48.588566-07:00
description: "Downloading a web page means grabbing data from the internet directly\
  \ into your app to display or process it. Programmers do it to obtain real-time\u2026"
lastmod: '2024-02-25T18:49:56.453591-07:00'
model: gpt-4-1106-preview
summary: "Downloading a web page means grabbing data from the internet directly into\
  \ your app to display or process it. Programmers do it to obtain real-time\u2026"
title: Downloading a web page
---

{{< edit_this_page >}}

## What & Why?

Downloading a web page means grabbing data from the internet directly into your app to display or process it. Programmers do it to obtain real-time information or provide dynamic content to users.

## How to:

Elm requires side effects like HTTP requests to be structured as commands. You'll use the `Http` module to fetch and handle the response.

```Elm

module Main exposing (main)

import Browser
import Html exposing (Html, text)
import Http

type alias Model =
    { content : String }

type Msg
    = GotText (Result Http.Error String)

init : ( Model, Cmd Msg )
init =
    ( Model ""
    , fetchPage "https://api.example.com/data"
    )

fetchPage : String -> Cmd Msg
fetchPage url =
    Http.get { url = url, expect = Http.expectString GotText }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText (Ok data) ->
            ( { model | content = data }, Cmd.none )

        GotText (Err _) ->
            ( { model | content = "Error: Could not fetch page." }, Cmd.none )

view : Model -> Html Msg
view model =
    text model.content

main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }

```

Given a successful fetch, `content` in your model will hold the page's contents. On error, it'll contain a simple error message.

## Deep Dive

Elm treats side effects as a Data, which means HTTP requests are managed by the Elm runtime, not directly in your code. Historically, this was a departure from languages like JavaScript, where side effects are more free-wheeling. Alternatives in other languages might be `fetch` in JavaScript or Python's `requests`. Elm's approach ensures your app remains predictable and maintainable by encoding side effects into types and using a centralized `update` function to manage changes.

The `Http` module didn't always exist in Elm. Early versions rolled their own AJAX, which was cumbersome. Now, `Http` provides a suite of functions to handle various cases, like expecting JSON or strings, which makes it more user-friendly.

Implementation-wise, when you call `fetchPage`, Elm sends a message to your `update` function with the result. It will either be `Ok data` if it succeeds or `Err error` if it fails. You pattern-match on these outcomes and update your `Model` and view accordingly.

## See Also

- Elm's HTTP package documentation: [https://package.elm-lang.org/packages/elm/http/latest/](https://package.elm-lang.org/packages/elm/http/latest/)
- Elm Guide on Effects: [https://guide.elm-lang.org/effects/](https://guide.elm-lang.org/effects/)
- JSON Decoding in Elm (for when the data you're fetching isn't a plain string): [https://package.elm-lang.org/packages/elm/json/latest/](https://package.elm-lang.org/packages/elm/json/latest/)
