---
title:                "Elm: Wyszukiwanie i zamiana tekstu"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego
Co skłania nas do szukania i zamieniania tekstu? Czasami może być to konieczne, gdy chcemy aktualizować istniejący kod lub poprawiać błędy. Może to także pomóc w ujednoliceniu tekstu w naszym kodzie, co ułatwi jego czytanie i zrozumienie.

## Jak to zrobić
W Elm istnieją dwa sposoby na wyszukiwanie i zamienianie tekstu. Pierwszym jest użycie funkcji `String.replace`, która przyjmuje string wejściowy, wzorzec do wyszukania i nowy tekst do zastąpienia. Na przykład:

```elm
String.replace "world" "Elm" "Hello world!" -- zwróci "Hello Elm!"
```

Drugim sposobem jest użycie wyrażeń regularnych za pomocą funkcji `Regex.replace`. Na przykład:

```elm
Regex.replace (Regex.regex "world") "Elm" "Hello world!" -- zwróci "Hello Elm!"
```

Obie te funkcje zwrócą nowy string z zastąpionym tekstem.

## Głębsze zagłębienie
Podczas pisania kodu w Elm istnieje również możliwość użycia wyszukiwania i zamieniania tekstu w plikach konfiguracyjnych, na przykład Json. W tym przypadku zaleca się użycie modułu `Json.Decode.Pipeline`, który udostępnia funkcję `replace` do wyszukiwania i zamieniania tekstu w strukturach Json. 

Przykład użycia:

```elm
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)

configDecoder : Decoder Config
configDecoder =
  pipeline Config
    |> required "username" string
    |> required "password" (newDecoder "H@6" string)
    |> replace "H@6" "SECRET"
```

W powyższym przykładzie, jeśli w pliku konfiguracyjnym znajdzie się wartość "H@6" w polu "password", zostanie ona automatycznie zastąpiona przez "SECRET".

## Zobacz także
- Dokumentacja funkcji `String.replace` w Elm: https://package.elm-lang.org/packages/elm/core/latest/String#replace
- Dokumentacja funkcji `Regex.replace` w Elm: https://package.elm-lang.org/packages/elm/regex/latest/Regex#replace
- Dokumentacja modułu `Json.Decode.Pipeline` w Elm: https://package.elm-lang.org/packages/NoRedInk/elm-decode-pipeline/latest/Json-Decode-Pipeline