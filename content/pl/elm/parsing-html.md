---
title:                "Analiza składniowa HTML"
html_title:           "Gleam: Analiza składniowa HTML"
simple_title:         "Analiza składniowa HTML"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/parsing-html.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Rozbieranie HTML, to proces, który pozwala na wyodrębnienie konkretnych danych i informacji z kodu HTML. Programiści robią to, aby przeanalizować struktury stron internetowych, pozyskać potrzebne dane lub przekształcić je w inny, bardziej użyteczny format.

## Jak to zrobić:

```Elm
import Html.Parser
import Html.Parser.Util

exampleHtml : String
exampleHtml =
    """
    <p>Witaj Świecie!</p>
    """

parseHello : String -> Maybe String
parseHello htmlText =
    Html.Parser.parse htmlText
        |> Result.andThen (Html.Parser.Util.select "p")
        |> Result.toMaybe
        |> Maybe.andThen List.head
        |> Maybe.andThen .innerHtml
```

Naszym wynikiem po zastosowaniu `parseHello exampleHtml` będzie `Just "Witaj Świecie!"`.

## Deep Dive

Rozbieranie HTML ma swoje korzenie w początkach internetu, kiedy to statyczne strony HTML były podstawą komunikacji sieciowej. Dziś, z nadejściem dynamicznych aplikacji internetowych, możemy pracować z HTML na bardzo różne sposoby.

Alternatywą dla Html.Parser jest `Html.Parser2`, który oferuje więcej elastyczności, ale jest mniej intuicyjny.

Podczas implementacji "parsing" HTML w Elm, ważne jest zrozumienie, jak działa monad `Maybe` i `Result`. Te typy danych mają centralne znaczenie dla obsługi błędów w czasie parsowania, a doświadczenie z nimi jest bezcenne podczas pracy z Html.Parser.

## Zobacz także:

- Dokumentacja Elm: https://elm-lang.org/docs
- Html.Parser w Elm: https://package.elm-lang.org/packages/elm/html/latest/Html-Parser
- Html.Parser2 w Elm: https://package.elm-lang.org/packages/eeue56/elm-html-parser/latest/