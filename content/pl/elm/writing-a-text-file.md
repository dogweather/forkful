---
title:                "Zapisywanie pliku tekstowego"
html_title:           "Arduino: Zapisywanie pliku tekstowego"
simple_title:         "Zapisywanie pliku tekstowego"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
(Po co i dlaczego?)

Zapisywanie pliku tekstowego to proces tworzenia albo modyfikowania danych tekstowych w pliku na dysku. Programiści robią to, aby zachować dane, które mogą być później odczytane przez ludzi lub inne programy.

## How to:
(Jak to zrobić?)

Elm nie zapewnia bezpośredniego API do zapisywania plików ze względu na jego ograniczenia w interakcji z systemem plików. Jednakże, możesz użyć JavaScript interop, znane jako porty, aby zapisywać pliki tekstowe w aplikacji webowej. Poniżej znajdziesz przykładowy kod.

```Elm
port module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

-- Definicja portu do zapisywania tekstowego pliku
port saveFile : String -> Cmd msg

-- Widok, przycisk zapisujący określony tekst do pliku
view : Html msg
view =
    div []
        [ button [ onClick (saveFile "Tekst do zapisu") ] [ text "Zapisz plik" ] ]

-- Podstawowa inicjalizacja aplikacji Elm
main =
    Browser.sandbox { init = (), view = view, update = \_ _ -> () }
```

JavaScript po drugiej stronie portu może wyglądać następująco:

```JavaScript
// Subskrybcja portu Elm w JavaScript
app.ports.saveFile.subscribe(function(text) {
    // Implementacja zapisywania pliku tekstowego
    var blob = new Blob([text], {type: "text/plain;charset=utf-8"});
    saveAs(blob, "example.txt"); // Używając biblioteki FileSaver.js
});
```

## Deep Dive:
(Więcej informacji)

Historia: Elm został stworzony dla bezpieczeństwa i niezawodności aplikacji front-endowych, więc dostęp do systemu plików jest ograniczony.

Alternatywy: Oprócz portów Elm i JavaScript, można użyć Web API, takich jak FileSaver.js lub natywne HTML5 `<a>` z atrybutem `download`, aby zainicjować pobieranie pliku.

Szczegóły implementacji: Port w Elm to bezpieczny sposób na komunikację z JavaScript. Porty wysyłają komunikaty między Elm a JS, umożliwiając wykonanie operacji związanych z systemem plików.

## See Also:
(Zobacz też)

- Oficjalna dokumentacja Elm o portach: [Elm Ports](https://guide.elm-lang.org/interop/ports.html)
- Repozytorium FileSaver.js na GitHub: [FileSaver.js](https://github.com/eligrey/FileSaver.js)
- Dokumentacja o atrybucie `download`: [HTML <a> download Attribute](https://www.w3schools.com/tags/att_a_download.asp)
