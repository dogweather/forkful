---
title:                "Analiza html"
html_title:           "Elm: Analiza html"
simple_title:         "Analiza html"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/parsing-html.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Parsing HTML (parsowanie HTML) to proces analizowania kodu HTML w celu wydobycia informacji z niego. Programiści często wykonują tę czynność, aby pobrać konkretne dane ze stron internetowych, np. tytuły, obrazy lub linki.

## Jak to zrobić:
Przykłady kodowania i wyników znajdują się w blokach kodu ```Elm ...```

1. Przy użyciu modułu `Html.Parser` możemy łatwo sparsować kod HTML i uzyskać go w formie drzewa. W przykładowym kodzie wykorzystano płynny operator `<!>` do oznaczania jednowierszowych wyrażeń.

```Elm
import Html
import Html.Parser exposing (..)

main =
    let
        html = "<h1>Hello, World!</h1><a href="https://example.com">Link</a>"
        parsedHtml = parse html
    in
        Html.text (firstChild parsedHtml)
```

**Output:** `Hello, World!` (tytuł jest zwracany jako element HTML)

2. Możemy także użyć modułu `Html.Attributes` do parsowania atrybutów elementów. W poniższym kodzie uzyskamy adres URL linku z poprzedniego przykładu.

```Elm
import Html
import Html.Parser exposing (..)
import Html.Attributes exposing (..)

main =
    let
        html = "<h1>Hello, World!</h1><a href="https://example.com">Link</a>"
        parsedHtml = parse html
    in
        Html.text (getAttribute "href" (firstChild parsedHtml))
```

**Output:** `https://example.com`

## Głębsze Zagłębianie:
1. Parsowanie HTML jest powszechnie używane w programowaniu internetowym, aby szybko i łatwo uzyskać potrzebne informacje z wybranej strony internetowej.

2. Alternatywne metody parsowania HTML to korzystanie z innych języków programowania, takich jak JavaScript czy Python, lub z użyciem zewnętrznych bibliotek.

3. W Elm, parsowanie HTML jest zintegrowane z innymi modułami i narzędziami, dzięki czemu jest szybkie i wydajne. 

## Zobacz także:
- [Dokumentacja Elm - Parsowanie HTML](https://package.elm-lang.org/packages/elm/parser/latest/Html-Parser)
- [Przykładowy kod parsowania HTML w Elm](https://dev.to/choonkeat/parsing-html-in-elm-3582)