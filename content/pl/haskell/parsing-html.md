---
title:                "Analiza składniowa HTML"
html_title:           "Gleam: Analiza składniowa HTML"
simple_title:         "Analiza składniowa HTML"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Analiza składniowa HTML - to proces ekstrakcji danych ze struktury HTML. Jest niezbędna dla programistów, aby uzyskać specyficzne informacje z dokumentów internetowych.

## Jak to zrobić:

Haskell ma kilka bibliotek do parsowania HTML, ale skupimy się na jednej zwanej `tagsoup`. Poniżej znajduje się przykładowy kod.

```Haskell
import Text.HTML.TagSoup

main = do
    let htmlCode = "<html><body><p>Hello, World!</p></body></html>"
    let tags = parseTags htmlCode
    print tags
```

Przykładowe wyjście to:

```Haskell
[TagOpen "html" [],TagOpen "body" [],TagOpen "p" [],TagText "Hello, World!",TagClose "p",TagClose "body",TagClose "html"]
```

## Głębsze zrozumienie:

### Historyczne kontekst:

Haskell, zamiast zwracać błędy podczas analizy składniowej, zwraca listę znaczników, które programista może następnie przefiltrować lub analizować według własnych potrzeb. Taka strategia wywodzi się z filozofii języka skoncentrowanej na funkcjach.

### Alternatywy:

Innymi bibliotekami do analizy składniowej HTML w języku Haskell są `html-conduit` i `pandoc`. Każde z nich oferuje różne zestawy funkcji i abstrakcje, które możemy dopasować do naszych potrzeb.

### Szczegóły implementacji:

`tagsoup` pozwala na analizę składniową dowolnego tekstu, nie tylko kodu HTML. Parsuje on również używając techniki leniwego przetwarzania, co zwiększa wydajność podczas pracy z dużymi plikami HTML.

## Zobacz też:

1. Dokumentacja `tagsoup` na Hackage: http://hackage.haskell.org/package/tagsoup
2. Poradnik Haskell na Stack Overflow: https://stackoverflow.com/tags/haskell/info
3. Blog o wyciąganiu danych z HTML za pomocą Haskell: https://chrispenner.ca/posts/tagsoup