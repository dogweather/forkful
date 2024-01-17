---
title:                "Generowanie losowych liczb."
html_title:           "Kotlin: Generowanie losowych liczb."
simple_title:         "Generowanie losowych liczb."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

Pozopcjaalajac zwariowaae liczby w Kotlinie

## Cto & Dla czego?

Generowanie losowych liczb jest procesem tworzenia - jak sama nazwa wskazuje - liczb losowych. Programiści często korzystają z tego narzędzia, aby symulować rzeczywistość lub generować losowe dane do testowania swoich programów.

## W jaki sposob?

W Kotlinie istnieje wiele sposobów generowania losowych liczb. Jedną z nich jest użycie funkcji ```random()```, która zwraca losową liczbę z przedziału od 0 do 1. Możemy również określić przedział, np. ```random(1..10)``` zwróci nam losową liczbę z przedziału od 1 do 10. 

Możemy także wykorzystać bibliotekę ```kotlin.random``` do zaawansowanych operacji związanych z liczbami losowymi, takich jak generowanie tablicy losowych liczb czy ustawianie ziarna.

## Głębokie zanurzenie

Idea generowania losowych liczb jest znana od dawna i jest szeroko wykorzystywana w programowaniu i matematyce. Jedną z alternatyw dla funkcji ```random()``` jest użycie generatorów liczb pseudolosowych, które są oparte na algorytmach i mogą generować sekwencje liczb, które wydają się losowe, ale w rzeczywistości są deterministyczne.

Implementacja działania funkcji ```random()``` różni się między różnymi językami programowania, jednak ważne jest, aby zapewnić, że wygenerowane liczby są w rzeczywistości losowe i nie występują w niej żadne wzorce.

## Zobacz także

- Dokumentacja Kotlin: https://kotlinlang.org/docs/reference/java-interop.html
- Generator liczb pseudolosowych: https://www.geeksforgeeks.org/pseudo-random-number-generator-prng/
- Informacje o historii generowania losowych liczb: https://pl.wikipedia.org/wiki/Generator_liczb_pseudolosowych