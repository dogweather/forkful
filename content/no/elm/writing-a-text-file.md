---
title:                "Skriving av en tekstfil"
html_title:           "Arduino: Skriving av en tekstfil"
simple_title:         "Skriving av en tekstfil"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å skrive en tekstfil er å lagre tekstdata til en fil på disken. Programmerere gjør dette for å lagre data som app-innstillinger, logger eller å eksportere brukerdata.

## Hvordan:
Elm har ikke direkte filsystem-tilgang, men du kan bruke Elm til å forberede filinnhold som brukeren kan laste ned. Her er et eksempel:

```Elm
module Main exposing (..)
import Browser
import Html exposing (Html, button, text)
import Html.Attributes exposing (download, href)
import Html.Events exposing (onClick)
import Url

main =
    Browser.sandbox { init = init, update = update, view = view }

type alias Model = String

init : Model
init =
    "Hei, dette er tekst som skal bli til en fil."

type Msg = Download

update : Msg -> Model -> Model
update _ model = model

view : Model -> Html Msg
view model =
    let
        encoded = Url.percentEncode model
        dataUrl = "data:text/plain;charset=utf-8," ++ encoded
    in
    button [ onClick Download, download "minfil.txt", href dataUrl ] [ text "Last ned filen" ]
```

Når du trykker "Last ned filen", blir teksten i `init` nedlastet som `minfil.txt`.

## Dypdykk
Elm er designet for frontend-webutvikling og har ikke innebygget filsystem-tilgang som server-språk. For å lagre filer må man bruke web-API-er, som i eksempelet over. Det er nyttig å bruke `Url.percentEncode` for å sørge for at tekstinnholdet er URL-kompatibelt. Alternativer, som å poste til en server, finnes, men er utenfor Elm sin direkte funksjon.

## Se Også
- Elm dokumentasjon: https://package.elm-lang.org/packages/elm/browser/latest/
- MDN Web Docs om "data" URL schema: https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/Data_URIs
