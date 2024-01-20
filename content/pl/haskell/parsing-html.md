---
title:                "Przetwarzanie HTML"
date:                  2024-01-20T15:31:54.755875-07:00
html_title:           "Bash: Przetwarzanie HTML"
simple_title:         "Przetwarzanie HTML"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Parsing HTML to proces wydobywania danych z dokumentów HTML. Programiści robią to, by manipulować, analizować lub ekstrahować specyficzne informacje z stron internetowych.

## How to: (Jak to zrobić:)
Haskell posiada kilka bibliotek do parsowania HTML, ale skupimy się na `hxt`, która jest potężna i wygodna w użyciu.

```Haskell
{-# LANGUAGE Arrows #-}

import Text.XML.HXT.Core

main :: IO ()
main = do
    -- Załóżmy, że 'example.html' zawiera HTML do sparsowania
    runX $ readDocument [withParseHTML yes, withWarnings no] "example.html"
         >>> deep (isElem >>> hasName "a" >>> getAttrValue "href")
         >>= print
```
Wyjście to lista URL-i, które są wartościami atrybutu href dla tagów anchor (`<a>`).

## Deep Dive (Głębsze spojrzenie)
Początki bibliotek do parsowania HTML w Haskellu sięgają lat, kiedy język ten zyskiwał na popularności wśród entuzjastów programowania funkcyjnego. `hxt` wykorzystuje koncepcję arrowów, oferując wyrafinowany, ale czytelny sposób pracy z XML i HTML.

Alternatywą dla `hxt` może być `tagsoup`, prościej podejście, które jest mniej rygorystyczne co do poprawności parsowanego HTML.

Parsowanie HTML polega na konwersji stringów (lub strumieni bajtów) na strukturę danych, zazwyczaj drzewo, co ułatwia wyszukiwanie elementów i atrybutów.

## See Also (Zobacz też)
- HXT tutorial: http://haskell.github.io/hxt/
- TagSoup: http://hackage.haskell.org/package/tagsoup
- Haskell XML Toolbox (HXT): http://hackage.haskell.org/package/hxt
- "Real World Haskell" (rozdział o parsowaniu XML/HTML): http://book.realworldhaskell.org/read/programming-with-monads.html