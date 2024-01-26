---
title:                "Interpolacja łańcuchów znaków"
date:                  2024-01-20T17:50:45.349516-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolacja łańcuchów znaków"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Interpolacja łańcuchów pozwala wpleść wartości zmiennych w tekst. Programiści robią to, by dynamicznie generować treść – dzięki temu kod jest bardziej elastyczny i czytelny.

## How to (Jak to zrobić):
Clojure używa funkcji `str` do łączenia stringów, ale nie ma wbudowanej interpolacji. Możesz to osiągnąć za pomocą `format` albo biblioteki jak `clojure.string`:

```Clojure
;; Z użyciem format
(def name "Alicja")
(println (format "Cześć, %s!" name)) ; Wypisze: Cześć, Alicja!

;; Z użyciem clojure.string/interpose, by wpleść separat
(require '[clojure.string :as str])
(println (str/join ", " ["witaj" "świecie"])) ; Wypisze: witaj, świecie
```

## Deep Dive (Dogłębna analiza)
Interpolacja łańcuchów nie jest natywnie obsługiwana w Clojure, w przeciwieństwie do języków jak Ruby czy Python. To dlatego trzeba używać `format` albo bibliotek zewnętrznych. `format` jest swego rodzaju mostem między Clojure a Javą – wykorzystuje Java's `String.format`. 

Alternatywy:
- `str` łączy stringi, ale bez formatowania.
- macro z `clojure.core/for` czy inne sztuczki makr mogą symulować interpolację.

Szczegóły implementacyjne:
- `format` obsługuje różne specyfikatory formatowania, więc masz kontrolę nad wyjściem.
- `clojure.string` jest częścią standardowej biblioteki Clojure i oferuje dodatkowe operacje na stringach.

## See Also (Zobacz również)
- Dokumentacja `clojure.string`: https://clojuredocs.org/clojure.string
- Przykłady użycia `format`: https://clojuredocs.org/clojure.core/format
- Porównanie stringów w Clojure: https://www.braveclojure.com/do-things/#3_5
