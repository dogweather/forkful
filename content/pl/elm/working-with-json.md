---
title:                "Praca z formatem JSON"
html_title:           "Elm: Praca z formatem JSON"
simple_title:         "Praca z formatem JSON"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/working-with-json.md"
---

{{< edit_this_page >}}

## Czym jest i dlaczego to robimy?

Praca z JSON to nic innego jak przetwarzanie danych w formacie JavaScript Object Notation, czyli formatu wymiany danych opartego na składni JavaScript. Programiści wykorzystują ten format, ponieważ jest on lekki, czytelny dla człowieka oraz łatwy do przetwarzania przez komputery.

## Jak to zrobić:

Przykładowe użycie Elm do przetwarzania i wyświetlania odpowiedzi JSON:

``` Elm
import Http
import Json.Decode exposing (..)

type alias Post =
    { userId : Int
    , id : Int
    , title : String
    , body : String
    }

getPosts : Cmd Msg
getPosts =
    Http.get
        { url = "https://jsonplaceholder.typicode.com/posts"
        , expect = Http.expectJson GotPosts postDecoder
        }

postDecoder : Decoder (List Post)
postDecoder =
    list
        Post
        |> field "userId" int
        |> field "id" int
        |> field "title" string
        |> field "body" string

update model msg =
    case msg of
        GotPosts (Ok posts) ->
            ( { model | posts = posts }, Cmd.none )

        GotPosts (Err _) ->
            ( model, Cmd.none )
```

Przykładowy wynik wywołania funkcji `getPosts`:
``` Elm
[{ userId = 1
  , id = 1
  , title = "sunt aut facere repellat provident occaecati excepturi optio reprehenderit"
  , body = "quia et suscipit suscipit ..."
  }, { userId = 1
  , id = 2
  , title = "qui est esse"
  , body = "est rerum tempore vitae ..."}]
```

## Głębsze wstrząśnięcie:

Format JSON został zaprojektowany przez Douglas Crockford w latach 90. jako sposób na wymianę danych między aplikacjami internetowymi. Alternatywami dla Elm w przetwarzaniu JSON są inne języki programowania takie jak JavaScript, Python czy Java. W Elm wykorzystuje się dekodery do przekształcania danych JSON na struktury danych wewnątrz języka.

## Zobacz również:

- Dokumentacja Elm: https://elm-lang.org/docs/json
- Wprowadzenie do JSON: https://www.json.org/json-pl.html