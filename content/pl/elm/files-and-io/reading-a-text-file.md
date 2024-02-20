---
date: 2024-01-20 17:54:23.896483-07:00
description: "Czytanie plik\xF3w tekstowych polega na za\u0142adowaniu ich zawarto\u015B\
  ci do programu. Programi\u015Bci robi\u0105 to, \u017Ceby przetwarza\u0107 dane,\
  \ konfigurowa\u0107 aplikacje lub\u2026"
lastmod: 2024-02-19 22:04:54.470080
model: gpt-4-1106-preview
summary: "Czytanie plik\xF3w tekstowych polega na za\u0142adowaniu ich zawarto\u015B\
  ci do programu. Programi\u015Bci robi\u0105 to, \u017Ceby przetwarza\u0107 dane,\
  \ konfigurowa\u0107 aplikacje lub\u2026"
title: Odczytywanie pliku tekstowego
---

{{< edit_this_page >}}

## What & Why?

Czytanie plików tekstowych polega na załadowaniu ich zawartości do programu. Programiści robią to, żeby przetwarzać dane, konfigurować aplikacje lub ładować zasoby.

## How to:

Elm aktualnie nie obsługuje bezpośredniego odczytu plików z dysku ze względów bezpieczeństwa i filozofii języka. Jednak możesz odczytać pliki tekstowe przesłane przez użytkownika przy użyciu `File` i `FileReader` API dostępnych w przeglądarce. Oto przykład:

```Elm
module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (on, targetValue)
import Html.Attributes exposing (type')
import File exposing (File)
import File.Selector exposing (file)
import File.Reader exposing (readAsText)

type Msg
    = SelectFile (List File)
    | FileLoaded (Result FileReader.Error String)

main =
    Browser.sandbox { init = init, update = update, view = view }

init =
    { content = "" }

update msg model =
    case msg of
        SelectFile files ->
            case files of
                file :: _ ->
                    ( model, readAsText file FileLoaded )

                [] ->
                    ( model, Cmd.none )

        FileLoaded (Ok fileContent) ->
            { model | content = fileContent }

        FileLoaded (Err _) ->
            ( model, Cmd.none )

view model =
    div []
        [ input [ type' "file", on "change" (Json.Decode.map SelectFile targetValue |> file) ] []
        , pre [] [ text model.content ]
        ]
```

Sample output depends on the file content - it displays the content inside a `pre` element after selecting a file.

## Deep Dive

Początkowo Elm został zaprojektowany tak, aby skupić się na front-endzie bez bezpośredniej interakcji z systemem plików. To część większej decyzji zapewnienia bezpiecznego, przewidywalnego środowiska do pisania aplikacji webowych. Alternatywy? Możesz użyć portów do komunikacji z JavaScriptem dla operacji po stronie serwera czy też użyć lokalnego przechowywania, jak `localStorage` do trzymania danych. Implementacja FileReader'a w Elm wykorzystuje pod spodem natywny mechanizm podobny do tego z JavaScriptu, ale zapakowany w bardziej funkcyjne i bezpieczne API.

## See Also

- Elm File package: https://package.elm-lang.org/packages/elm/file/latest/
- Elm File.Reader package: https://package.elm-lang.org/packages/elm/file/latest/File-Reader
- Elm Guide on JavaScript Interop (Ports): https://guide.elm-lang.org/interop/
