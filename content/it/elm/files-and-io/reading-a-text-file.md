---
date: 2024-01-20 17:54:34.827225-07:00
description: "How to: Elm, essendo focalizzato sulla sicurezza, non permette l'accesso\
  \ diretto ai file del sistema dall'interno del browser. Per leggere un file, devi\u2026"
lastmod: '2024-03-13T22:44:43.366811-06:00'
model: gpt-4-1106-preview
summary: Elm, essendo focalizzato sulla sicurezza, non permette l'accesso diretto
  ai file del sistema dall'interno del browser.
title: Lettura di un file di testo
weight: 22
---

## How to:
Elm, essendo focalizzato sulla sicurezza, non permette l'accesso diretto ai file del sistema dall'interno del browser. Per leggere un file, devi utilizzare una `input` HTML con `type="file"` e intercettare l'evento in Elm.

```Elm
-- Main.elm
module Main exposing (..)
import Browser
import File exposing (File)
import File.Select as FileSelect
import Html exposing (Html, button, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)

type Msg
    = SelectFile
    | FileSelected File
    | FileReaderResult (Result () String)

type alias Model =
    { fileContent : Maybe String }

main =
    Browser.element
        { init = \_ -> (Model Nothing, Cmd.none)
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

view model =
    -- Bottone che permette la selezione del file
    button [ onClick SelectFile] [ text "Select a text file" ]

update msg model =
    case msg of
        SelectFile ->
            ( model
            , FileSelect.file [ "text/plain" ] FileSelected
            )

        FileSelected file ->
            ( model
            , File.reader FileReaderResult file
            )

        FileReaderResult (Ok content) ->
            ( { model | fileContent = Just content }
            , Cmd.none
            )

        FileReaderResult (Err _) ->
            ( { model | fileContent = Just "Failed to read the file." }
            , Cmd.none
            )
```

Questo snippet di codice Elm mostra un modo per selezionare e leggere un file di testo. Dall'HTML, l'utente può caricare un file e Elm lo legge, visualizzando infine il contenuto.

## Deep Dive
In passato, Elm aveva più funzioni per interagire con il filesystem, ma ora, per ragioni di sicurezza e per aderire ai paradigmi del web moderno, questa interazione è gestita con l'Html e JavaScript. Puoi integrare Elm con JavaScript tramite approcci come i `Port`, ma attenzione, devi seguirlo nel tuo codice JavaScript.

Riguardo alle alternative, potresti pensare ad usare Node.js per script che girano lato server con accesso completo al filesystem, oppure potresti usare WebAssembly per operazioni più complesse e performanti.

Gli sviluppatori scelgono Elm per le interfacce utente nel browser per la sua robustezza e facilità di manutenzione. La lettura di file di testo resta una funzionalità chiave per molte applicazioni web, ma in Elm è approcciata in modo molto controllato per rafforzare la sicurezza.

## See Also
- Elm's official guide on file uploads: https://guide.elm-lang.org/interop/flags.html
- Elm file package documentation: https://package.elm-lang.org/packages/elm/file/latest/
- Working with JavaScript in Elm through ports: https://elm-lang.org/docs/ports
