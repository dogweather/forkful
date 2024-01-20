---
title:                "Pobieranie strony internetowej"
html_title:           "C#: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Pobieranie strony internetowej to proces, w którym twoja aplikacja pobiera dane HTML z konkretnej strony internetowej. Programiści robią to by zdobyć dane z różnych stron, które można potem wykorzystać w aplikacji.

## Jak to zrobić:

Podstawowym sposobem na pobranie strony internetowej w Elm jest użycie funkcji `Http.get`.

```Elm
import Http
import Json.Decode as Decode

getPage : String -> Cmd Msg
getPage url =
    Http.get
        { url = url
        , expect = Http.expectString GotPage
        }

type Msg =
    GotPage (Result Http.Error String)
```

Aplikacja będzie wywoływać funkcję `getPage` z adresem URL strony, którą chcesz pobrać. Oczekiwany wynik to otrzymane dane strony.

## Głębsze Zagadnienia:

- Kontekst historyczny: Elm to funkcyjny język programowania do tworzenia webowych aplikacji interaktywnych. Został zaprojektowany tak, aby unikać błędów wykonania na produkcji, co czyni go idealnym dla rozwoju złożonych aplikacji internetowych.

- Alternatywy: istnieją inne biblioteki do pobierania stron internetowych w Elm, na przykład `elm-http-builder`, które oferują większą elastyczność.

- Szczegóły implementacji: Elm wykorzystuje monadę `Cmd` do obsługi efektów ubocznych takich jak żądania HTTP. Ta implementacja różni się od większości języków JavaScript, które używają obietnic (promises) lub callbacków do obsługi asynchroniczności.

## Zobacz także:

- Elm: http://elm-lang.org
- Http.get: http://package.elm-lang.org/packages/elm/http/latest/Http#get
- JSON decoding: http://guide.elm-lang.org/interop/ports.html
- Http Builder: http://package.elm-lang.org/packages/lukewestby/elm-http-builder/latest/