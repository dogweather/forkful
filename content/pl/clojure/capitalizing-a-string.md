---
title:                "Zmiana wielkości liter w ciągu znaków"
html_title:           "Clojure: Zmiana wielkości liter w ciągu znaków"
simple_title:         "Zmiana wielkości liter w ciągu znaków"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, dlaczego czasem słowa w zdaniach są zapisane wielką literą? To nazywane jest kapitalizacją i może pomóc w czytaniu i zrozumieniu tekstu. W tym artykule dowiesz się, jak działa kapitalizacja w języku programowania Clojure i jak jej używać.

## Jak To Zrobić

```Clojure
(defn capitalize-word 
  [string] 
  (str (clojure.string/capitalize string))) 

(capitalize-word "january") ;; "January"
(capitalize-word "MAY") ;; "May"
(capitalize-word "august") ;; "August"
```

Funkcja ```capitalize-word``` przyjmuje argument w postaci stringa i zwraca kapitalizowane słowo. Używamy tutaj funkcji ```clojure.string/capitalize```, która jest częścią biblioteki standardowej Clojure i kapitalizuje pierwszą literę słowa. Następnie używamy funkcji ```str```, aby utworzyć nowy string z kapitalizowaną pierwszą literą. W przykładowych wywołaniach możemy zauważyć, że funkcja ta działa dla różnych wielkości liter.

## Pogłębiona Analiza

Aby lepiej zrozumieć, jak działa nasza funkcja, musimy przyjrzeć się nieco bliżej funkcji ```clojure.string/capitalize```. Ta funkcja działa na podstawie prymitywów Clojure - sekwencji. Słowo jest sekwencją, ponieważ składa się z poszczególnych liter, które mogą być traktowane jako elementy sekwencji. Funkcja ```clojure.string/capitalize``` zwraca nową sekwencję, która jest wynikiem zastosowania funkcji ```clojure.core/capitalize``` do każdego elementu sekwencji. Funkcja ```clojure.core/capitalize``` jest wywoływana w celu kapitalizacji pojedynczego znaku. Następnie obrabiamy tę sekwencję za pomocą funkcji ```str```, aby utworzyć nowy string.

## Zobacz Również

- Dokumentacja Clojure o funkcji ```str```: https://clojuredocs.org/clojure.string/str
- Dokumentacja Clojure o funkcji ```clojure.string/capitalize```: https://clojuredocs.org/clojure.string/capitalize
- Dokumentacja Clojure o funkcji ```clojure.core/capitalize```: https://clojuredocs.org/clojure.core/capitalize