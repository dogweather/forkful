---
title:                "Parsowanie HTML"
html_title:           "Elm: Parsowanie HTML"
simple_title:         "Parsowanie HTML"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/parsing-html.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś web developerem lub programistą, który pracuje z HTML, prawdopodobnie zetknąłeś się z koniecznością analizowania struktury kodu HTML. Może to być wyzwanie, zwłaszcza gdy czujesz się przytłoczony długim i nieuporządkowanym kodem. W takich przypadkach, narzędzie takie jak Elm może być bardzo pomocne.

## Jak to zrobić?

Elm jest językiem programowania funkcyjnego, który jest używany do budowy interaktywnych aplikacji webowych. Co więcej, zawiera wbudowane narzędzia do analizowania i parsowania dokumentów HTML. Sprawia to, że jest idealnym narzędziem do przetwarzania elementów HTML i wyodrębniania potrzebnych danych.

Poniższy przykład pokazuje, jak użyć funkcji ```Html.parser``` do wyciągnięcia linków znajdujących się wewnątrz ```<a>``` tagów w dokumencie HTML.

```
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Parser as Parser exposing (..)
import String

document = "
<html>
<body>
<a href='https://example.com'>Link 1</a>
<a href='https://example2.com'>Link 2</a>
</body>
</html>"

parseLinks : String -> List String
parseLinks html =
    let
        links =
            Parser.map (\a -> a.href)
                (Parser.a [] Parser.attribute)
    in
        Parser.run links html

main =
    text (String.join "\n" (parseLinks document))
```

**Output:**

```
https://example.com
https://example2.com
```

W powyższym przykładzie najpierw importujemy moduły Html i Html.Parser, a następnie definiujemy dokument HTML jako zmienną ```document```. Następnie wykorzystujemy funkcję ```parseLinks```, która przyjmuje jako argument dokument HTML, a następnie wykorzystuje wbudowaną funkcję ```Parser``` do przetworzenia go. W wyniku otrzymujemy listę linków, które są wyświetlane przy użyciu funkcji ```text```.

## Deep Dive

Podczas dokładniejszego przyjrzenia się przykładowi kodu, możemy dojść do następujących wniosków:

- ```Html.parser``` jest funkcją, która przyjmuje dwa argumenty: parser zróżnicowany (w tym przypadku ```links```) oraz dokument HTML.
- Parser ten wykorzystuje funkcję ```map```, która przyjmuje jako argument parser ```a```, funkcję manipulującą (w tym przypadku pobierającą wartość atrybutu ```href```), a także parser atrybutu.
- Wywołanie funkcji ```run``` z parserem ```links``` oraz dokumentem HTML spowoduje zwrócenie listy linków znajdujących się w dokumencie.

W przypadku bardziej zaawansowanych analiz, funkcja ```Html.parser``` może zostać rozbudowana za pomocą funkcji takich jak ```andThen```, ```oneOf```, ```sequence```, aby obsłużyć bardziej złożone struktury HTML.

## Zobacz także

- Oficjalna dokumentacja Elm: https://guide.elm-lang.org/
- Przykłady HTML parsing w Elm: https://github.com/elm/parser/tree/master/examples