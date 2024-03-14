---
date: 2024-01-20 17:50:45.349516-07:00
description: "Interpolacja \u0142a\u0144cuch\xF3w pozwala wple\u015B\u0107 warto\u015B\
  ci zmiennych w tekst. Programi\u015Bci robi\u0105 to, by dynamicznie generowa\u0107\
  \ tre\u015B\u0107 \u2013 dzi\u0119ki temu kod jest bardziej\u2026"
lastmod: '2024-03-13T22:44:34.980006-06:00'
model: gpt-4-1106-preview
summary: "Interpolacja \u0142a\u0144cuch\xF3w pozwala wple\u015B\u0107 warto\u015B\
  ci zmiennych w tekst. Programi\u015Bci robi\u0105 to, by dynamicznie generowa\u0107\
  \ tre\u015B\u0107 \u2013 dzi\u0119ki temu kod jest bardziej\u2026"
title: "Interpolacja \u0142a\u0144cuch\xF3w znak\xF3w"
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
