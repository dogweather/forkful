---
title:                "Konwertowanie ciągu znaków na małe litery."
html_title:           "Clojure: Konwertowanie ciągu znaków na małe litery."
simple_title:         "Konwertowanie ciągu znaków na małe litery."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w programowaniu musimy porównywać teksty, jednak różnice w wielkości liter mogą sprawić problem. Dlatego warto nauczyć się konwertować ciągi znaków na małe litery, co ułatwi nam porównywanie i przetwarzanie danych.

## Jak to zrobić?

```Clojure
(defn lowercase [str]
  (clojure.string/lower-case str))

(lowercase "CZEŚĆ") ; output: "cześć"
```

Możemy użyć funkcji `clojure.string/lower-case`, która zwraca tekst w postaci małych liter. Jeśli chcemy wykorzystać tę funkcję w naszym własnym kodzie, musimy najpierw zadeklarować przestrzeń nazw `clojure.string` za pomocą `require`. Następnie możemy wywołać funkcję `lower-case` na naszym ciągu znaków.

```Clojure
(def text "HEJ!")
(lowercase text) ; output: "hej!"
```

Jeśli mamy już zadeklarowaną przestrzeń nazw `clojure.string`, możemy po prostu wywołać funkcję `lower-case` bezpośrednio na tekście.

```Clojure
(require '[clojure.string :as str])

(str/lower-case "Żegnaj") ; output: "żeśniej"
```

Możemy także użyć bloków `let` do zapisania przekonwertowanego tekstu w zmiennej.

```Clojure
(let [tekst "WITAJ"]
  (-> tekst
      str/lower-case
      (print))) ; output: "witaj"
```

## Głębsze zagadnienia

Podczas konwertowania ciągu znaków na małe litery, musimy pamiętać o wyborze odpowiedniego algorytmu. W języku Clojure istnieją dwa podejścia do konwersji liter na małe: "uppercase" i "titlecase". W zależności od wymagań naszej aplikacji, musimy wybrać odpowiednią metodę.

### Metoda "uppercase"

Metoda "uppercase" zamienia wszystkie litery w tekście na wielkie, niezależnie od ich pierwotnego stanu.

```Clojure
(str/upper-case "Hej") ; output: "HEJ"
(str/upper-case "hej") ; output: "HEJ"
```

### Metoda "titlecase"

Metoda "titlecase" przywraca pierwszą literę każdego słowa w tekście na wielką, natomiast reszta liter pozostaje niezmieniona.

```Clojure
(str/title-case "hej cześć") ; output: "Hej Cześć"
```

### Wybór odpowiedniej metody

Jeśli chcemy porównywać teksty i zależy nam na tym, aby różnice w wielkości liter były uwzględnione, powinniśmy użyć metody "uppercase". Natomiast jeśli chcemy wyświetlać tekst użytkownikowi, lepiej wybrać metodę "titlecase", aby utrzymać odpowiedni format tekstu.

## Zobacz także

- [Dokumentacja języka Clojure](https://clojure.org/)
- [Funkcja lowercase w Clojure](https://clojuredocs.org/clojure.string/lower-case)