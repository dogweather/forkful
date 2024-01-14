---
title:    "Clojure: Znajdowanie długości ciągu znaków"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Dlaczego

Często w programowaniu pojawia się potrzeba obliczenia długości ciągu znaków. Może to być użyteczne przy walidacji danych podczas tworzenia formularzy, czy też przy przetwarzaniu tekstów w aplikacji. W tym artykule dowiesz się, jak w prosty sposób znaleźć długość ciągu znaków w języku Clojure.

## Jak to zrobić

Obliczenie długości ciągu znaków w języku Clojure jest bardzo proste. Wystarczy użyć funkcji "count", która zwraca ilość elementów w danej kolekcji, a ciąg znaków jest właśnie kolekcją. Przykładowy kod wyglądałby następująco:

```Clojure
(def text "To jest przykładowy ciąg znaków")
(count text)
```

Powyższy kod zwróci wynik 32, ponieważ ciąg znaków składa się z 32 liter. W przypadku gdy chcemy policzyć długość ciągu znajdującego się w zmiennej, również możemy użyć funkcji "count":

```Clojure
(def weather "Słońce")
(count weather)
```

W tym przypadku zmienna "weather" przechowuje ciąg "Słońce" i funkcja "count" zwróci wynik 6.

## Deep Dive

Funkcja "count" może przyjmować również inne typy danych, takie jak lista czy mapa. W przypadku listy zwraca ona ilość elementów znajdujących się na tej liście, a w przypadku mapy zwraca ilość par klucz-wartość. Warto również wspomnieć, że funkcja "count" jest bardzo wydajna, ponieważ nie iteruje po całym ciągu znaków, ale zwraca jego długość na podstawie metadanych.

## Zobacz również

- Dokumentacja funkcji "count": https://clojuredocs.org/clojure.core/count
- Poradnik dla początkujących w Clojure: https://www.braveclojure.com/getting-started/
- Przykłady wykorzystania funkcji "count": https://www.dotkam.com/2010/09/01/count-in-clojure/