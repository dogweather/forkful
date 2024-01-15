---
title:                "Przetwarzanie HTML"
html_title:           "Haskell: Przetwarzanie HTML"
simple_title:         "Przetwarzanie HTML"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli interesuje Cię programowanie i chcesz tworzyć skuteczne aplikacje webowe, musisz poznać podstawy parsingu HTML. Wprowadzi Cię to w świat tworzenia i przetwarzania struktur danych, które są powszechnie wykorzystywane w internecie.

## Jak to zrobić

Parsowanie HTML w języku Haskell może być prostsze niż myślisz! Wystarczy skorzystać z biblioteki "html-conduit", która zawiera wiele przydatnych funkcji do analizy i przetwarzania dokumentów HTML.

Przykładowo, chcąc pobrać zawartość elementu <title> z danej strony internetowej, możemy wykorzystać poniższy kod:

```Haskell
import Text.HTML.TagSoup

main = do
  tags <- parseTags <$> getContents
  let title = fromTagText . head $ dropWhile (~/= "<title>") tags
  print title
```

Powyższy kod wykorzystuje funkcje z biblioteki "html-conduit" do przetworzenia pobranego dokumentu HTML. Następnie, korzystając z funkcji "fromTagText" i "head", pobieramy zawartość elementu <title> i wyświetlamy ją na ekranie. Proste, prawda?

## Głębsze wody

Parsowanie HTML w Haskellu jest jednak nieco bardziej skomplikowane niż przykłady przedstawione powyżej. Wymaga ono znajomości parserów oraz monadycznych operacji. Jeśli chcesz zagłębić się w temat, warto zapoznać się z dokumentacją biblioteki "html-conduit" oraz przeczytać o parsowaniu w języku Haskell.

## Zobacz także

- https://hackage.haskell.org/package/html-conduit
- https://www.haskell.org/documentation/