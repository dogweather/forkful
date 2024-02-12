---
title:                "Analiza składniowa HTML"
aliases:
- /pl/haskell/parsing-html/
date:                  2024-02-03T19:12:20.178233-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analiza składniowa HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co & Dlaczego?

Parsowanie HTML w Haskellu pozwala na ekstrakcję danych, manipulowanie zawartością HTML lub programistyczne interakcje ze stronami internetowymi. Operacja ta jest niezbędna do zadań takich jak scraping stron internetowych, automatyczne testowanie aplikacji webowych oraz wydobywanie danych ze stron - wykorzystując silny system typów i paradygmaty programowania funkcyjnego Haskella, aby zapewnić solidny i zwięzły kod.

## Jak to zrobić:

Do parsowania HTML w Haskellu użyjemy biblioteki `tagsoup` ze względu na jej prostotę i elastyczność. Najpierw upewnij się, że zainstalowałeś bibliotekę, dodając `tagsoup` do pliku cabal Twojego projektu lub uruchamiając `cabal install tagsoup`.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.TagSoup

-- Przykładowy HTML do demonstracji
let sampleHtml = "<html><body><p>Ucz się Haskella!</p><a href='http://example.com'>Kliknij tutaj</a></body></html>"

-- Parsowanie HTML i filtrowanie linków (tagów a)
let tags = parseTags sampleHtml
let links = [fromAttrib "href" tag | tag <- tags, isTagOpenName "a" tag]

-- Wydrukowanie wyekstrahowanych linków
print links
```

Przykładowe wyjście:
```plaintext
["http://example.com"]
```

Dla bardziej zaawansowanych potrzeb parsowania HTML rozważ użycie biblioteki `pandoc`, szczególnie jeśli pracujesz z konwersją dokumentów. Jest wyjątkowo wszechstronna, ale wiąże się z większą złożonością:

```haskell
import Text.Pandoc

-- Zakładając, że masz załadowany dokument Pandoc (doc), np. z czytania pliku
let doc = ... -- Tutaj wprowadź swój dokument Pandoc

-- Konwersja dokumentu do łańcucha HTML
let htmlString = writeHtmlString def doc

-- Teraz możesz sparsować `htmlString` jak powyżej lub postępować zgodnie z własnymi wymaganiami.
```
Pamiętaj, że `pandoc` to znacznie większa biblioteka skoncentrowana na konwersji między licznymi formatami znaczników, więc użyj jej, jeśli potrzebujesz tych dodatkowych możliwości lub jeśli już zajmujesz się formatami dokumentów w swojej aplikacji.
