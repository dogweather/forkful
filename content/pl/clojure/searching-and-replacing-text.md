---
title:                "Clojure: Wyszukiwanie i zastępowanie tekstu"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek znalazłeś się w sytuacji, gdy konieczne było zmienienie wielu wystąpień pewnego tekstu w Twoim kodzie? Może był to literał, nazwa zmiennej lub funkcji. Przeprowadzenie tych zmian ręcznie może być bardzo uciążliwe i narażać na błędy. Dlatego warto poznać prosty sposób na znalezienie i zamianę tekstów w języku Clojure.

## Jak To Zrobić

Szukanie i zamiana tekstów w Clojure jest bardzo prosta i możliwa dzięki wykorzystaniu funkcji `clojure.string/replace`. Ta funkcja przyjmuje trzy argumenty: źródłowy ciąg znaków, wyszukiwany tekst oraz tekst zastępujący. Dzięki temu możesz w prosty sposób znaleźć i zmienić określone ciągi w swoim kodzie.

### Przykłady

```
(require '[clojure.string :as str])

(def str1 "Hello, world")
(str/replace str1 "world" "everyone")

; Wynik: "Hello, everyone"
```

```
(defn square [n]
  (* n n))

(def str2 "(def square [n]
  (* n n))")

(str/replace str2 "square" "power")

; Wynik: "(def power [n]
  (* n n))"
```

```
(defn multiply [a b]
  (* a b))

(def str3 "(defn multiply [a b]
  (* a b))")

(str/replace str3 "(* a b)" "(* b a)")

; Wynik: "(defn multiply [a b]
  (* b a))"
```

Jak widzisz, zmiana tekstu jest bardzo prosta i nie wymaga dużego wysiłku. Funkcja `replace` pozwala także na wykorzystanie wyrażenia regularnego jako wyszukiwanego tekstu.

### Wyjście

Jeśli funkcja `replace` znajdzie i zamieni jakiekolwiek wystąpienie wyszukiwanego tekstu, zwróci nowy ciąg znaków z wprowadzoną zmianą. W przeciwnym razie, zwróci niezmieniony ciąg.

## W Głębi

W funkcji `replace` możesz również wykorzystać opcjonalny trzeci argument - liczbę wystąpień, które chcesz zmienić. Domyślnie jest to `nil`, co oznacza, że wszystkie wystąpienia będą zmienione. Możesz jednak określić liczbę, np. `2`, aby zmienić tylko pierwsze dwa wystąpienia.

Funkcja `replace` jest dostępna w przestrzeni nazw `clojure.string` od wersji Clojure 1.4. W przeszłości wykorzystywano kombinację funkcji `split` i `join` do osiągnięcia tego samego efektu, ale funkcja `replace` znacznie ułatwia i przyspiesza ten proces.

## Zobacz Również

- [Dokumentacja funkcji `replace`](https://clojuredocs.org/clojure.string/replace)
- [Przykłady wykorzystania funkcji `replace`](https://gist.github.com/michalmarcinkowski/d258b85497ad49c2206a)
- [Porównanie funkcji `replace` z użyciem `split` i `join`](https://medium.com/@colynnux/clojure-string-replace-cc50bcb34e19)