---
title:                "Haskell: Analiza html"
simple_title:         "Analiza html"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## Dlaczego

Parsing HTML (czyli analizowanie struktury języka HTML) może być bardzo przydatne w wielu aplikacjach, takich jak web scraping czy generowanie dokumentów.

## Jak to zrobić

Zacznijmy od importowania potrzebnych bibliotek, takich jak `Text.HTML.TagSoup` i `Data.Text`:

```Haskell
import Text.HTML.TagSoup
import Data.Text
```

Następnie, przygotujmy przykładową stronę internetową w postaci tekstu:

```Haskell
htmlText = "<html> <head> <title>Przykładowa strona</title> </head> <body> <h1>Witaj!</h1> <div class=\"content\"> <p>Ta strona ma przykładowy tekst.</p> </div> </body> </html>"
```

Teraz możemy użyć funkcji `parseTags` z biblioteki `Text.HTML.TagSoup` do przekształcenia tekstu na listę tagów:

```Haskell
htmlTags = parseTags htmlText
```

Możemy także wykorzystać funkcję `canonicalizeTags` do usunięcia białych znaków i wyrównania tagów:

```Haskell
cleanHtmlTags = canonicalizeTags htmlTags
```

Aby przeszukać konkretne elementy na stronie, możemy skorzystać z funkcji `sections`, która odpowiada za dzielenie tagów na sekcje, takie jak `<head>` czy `<body>`:

```Haskell
pageHead = sections (~== "<head>") cleanHtmlTags
```

Możemy również wykorzystać funkcję `TagText`, aby znaleźć konkretne tagi według nazwy:

```Haskell
pageTitle = case pageHead of
                ((TagOpen _ attrs): _) -> fromAttrib "title" attrs
```

W naszym przykładzie, wartość `pageTitle` będzie równa "Przykładowa strona".

## Rzut okiem w głąb

Funkcja `parseTags` jest bardzo przydatna, ponieważ pozwala nam na łatwe manipulowanie tagami i tekstem wewnątrz nich. Możemy także wykorzystać funkcję `isTagOpen` do sprawdzania, czy dany tag jest otwierającym tagiem, oraz funkcję `innerText` do pobierania tekstu znajdującego się wewnątrz tagu.

Ważne jest również pamiętanie o sposobie, w jaki tagi są zagnieżdżone w dokumencie HTML. Jeśli potrzebujemy znaleźć konkretny tag, możemy wykorzystać funkcję `getAttribute` do pobrania jego atrybutów, na przykład `class` czy `id`.

## Zobacz także

- [Dokumentacja biblioteki Text.HTML.TagSoup](https://hackage.haskell.org/package/tagsoup/docs/Text-HTML-TagSoup.html)
- [Tutorial o parsowaniu HTML w Haskellu](https://wiki.haskell.org/Parsing_a_simple_imperative_language)
- [Przykładowe projekty wykorzystujące parsowanie HTML w Haskellu](https://github.com/topics/haskell-html-parsing)