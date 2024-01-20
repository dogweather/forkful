---
title:                "Downloading a web page"
html_title:           "Bash recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?

Downloading a web page means to grab the content of that page programmatically. This can be useful for things like web scraping, automated testing, or offline viewing.

## How to:

Elm doesn't directly let you download a webpage due to its strictly enforced architecture. However, through HTTP requests, we can retrieve the HTML content.

```Elm
import Http
import Html exposing (Html, text)
import Json.Decode as Decode

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type Msg
    = GotPage (Result Http.Error String)

init : ( String, Cmd Msg )
init =
    ( ""
    , Http.getString "https://elm-lang.org"
        |> Http.send GotPage
    )

update : Msg -> String -> ( String, Cmd Msg )
update msg model =
    case msg of
        GotPage result ->
            case result of
                Ok page ->
                    ( page, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

view : String -> Html Msg
view model =
    text model

subscriptions : String -> Sub Msg
subscriptions model =
    Sub.none
```
Output will be the HTML content of the page.

## Deep Dive

Historically, downloading web pages was a breeze. However, with Elm, due to its design goals to provide a delightful language with no runtime exceptions, some actions that bear potential errors are made impossible to be done directly. Downloading a web page is one such task, as it may lead to issues like network failure, wrong URLs, non-existent pages, and more.

An alternative to Elm for this task might be Javascript, which provides a more flexible web handling but also leaves more room for errors and unhandled exceptions.

The Elm example works by making a simple HTTP GET request to the page's URL, then sends the result to the Elm runtime. The Elm runtime invokes the update function, handling either a successful page download (GotPage Ok) or an error (GotPage Err).

## See Also

Checkout these resources:
- Elm's doc about [HTTP library](https://package.elm-lang.org/packages/elm/http/latest/)