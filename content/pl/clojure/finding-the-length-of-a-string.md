---
title:                "Clojure: Znajdowanie długości ciągu znaków"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Zdarza się, że w trakcie programowania musimy znaleźć długość tekstu, który został przekazany do naszej funkcji. Może to być potrzebne do obliczeń lub do wyświetlenia informacji użytkownikowi. W tym artykule dowiesz się, jak w języku Clojure obliczyć długość tekstu.

## Jak to zrobić?


```Clojure
(def tekst "Cześć!")
(count tekst) ; zwróci długość tekstu, w tym przypadku 6

(def imię "Anna")
(count imię) ; zwróci długość tekstu, czyli 4

;; Możemy także przekazać długi tekst bezpośrednio do funkcji count
(count "To jest długi tekst.") ; zwróci 21
```

Aby obliczyć długość tekstu, używamy wbudowanej funkcji `count`, która zwraca liczbę znaków w danym tekście. Możemy przekazać do niej zarówno zmienną ze zdefiniowanym tekstem, jak i wprowadzić tekst bezpośrednio w funkcji.

## Deep Dive

Funkcja `count` zwraca liczbę znaków w danym tekście. W przypadku, gdy dany tekst jest pusty, funkcja zwróci 0. Możemy także użyć tej funkcji do obliczenia długości kolekcji, takiej jak lista czy wektor. Wtedy zwróci ona liczbę elementów w danej kolekcji.

## Zobacz także

- Dokumentacja funkcji `count`: https://clojuredocs.org/clojure.core/count
- Inne przydatne funkcje do pracy z tekstem w Clojure: https://www.luminusweb.net/docs/strings.md
- Szybki przewodnik po języku Clojure: https://clojure.org/guides/getting_started