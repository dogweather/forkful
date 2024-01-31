---
title:                "Scrivere un file di testo"
date:                  2024-01-19
simple_title:         "Scrivere un file di testo"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Scrivere un file di testo significa salvare dati in formato leggibile. Programmatori lo fanno per persistere informazioni, configurazioni, o per creare documenti utilizzabili da altri programmi o utenti.

## How to:
Elm non gestisce direttamente la scrittura su file perché gira nel browser. Tuttavia, puoi creare un file di testo e farlo scaricare all'utente. Ecco come:

```Elm
module Main exposing (..)
import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (href, download)
import Html.Events exposing (onClick)
import Json.Encode as Encode

type Msg = Download

main =
    Browser.sandbox { init = (), update = update, view = view }

update msg () =
    ()

view () =
    let
        jsonData =
            Encode.string "Ciao mondo! Questo è un file di testo."

        dataUrl =
            "data:text/plain;charset=utf-8," ++ encodeURIComponent (Encode.encode 0 jsonData)
    in
    div []
        [ button [ href dataUrl, download "mioFile.txt", onClick Download ] [ text "Scarica il file di testo" ] ]

encodeURIComponent : String -> String
encodeURIComponent =
    JsNative.encodeURIComponent

```
Crei una URL con dati codificati e poi usi `href` e `download` per fornire un link. Cliccando il bottone, si scarica il file.

## Deep Dive
Elm è progettato per sicurezza, quindi non ha accesso diretto al filesystem dell'utente. Questo era standard anche in JavaScript, ma le moderne API e Node.js hanno cambiato la situazione. Alternatives includono l'uso di Elm con node e server-side scripting per scrivere su file. Gli eventi file-download sono implementati tramite l'interazione con JavaScript e l'HTML5 `download` attribute.

## See Also
- Elm Guide su interazioni con JavaScript: https://guide.elm-lang.org/interop/
- Documentazione HTML5 su attributo `download`: https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a#attr-download
- Pacchetti Elm per lavorare con file: https://package.elm-lang.org/packages/elm/file/latest/
