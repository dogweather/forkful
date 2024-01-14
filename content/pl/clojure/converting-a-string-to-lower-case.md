---
title:    "Clojure: Konwersja ciągu znaków na małe litery"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja tekstu na małe litery może być przydatna w różnych sytuacjach programistycznych, takich jak porównywanie tekstu bez uwzględniania wielkości liter lub sprawdzanie poprawności danych wejściowych.

## Jak to zrobić

Konwersję tekstu na małe litery w języku Clojure można wykonać za pomocą funkcji "lower-case". Przykład użycia wygląda następująco:

```Clojure
(lower-case "TEKST NA MAŁE LITERY") ; zwróci "tekst na małe litery"
```

Można również użyć tej funkcji w połączeniu z innymi funkcjami, aby uzyskać bardziej skomplikowany wynik. Na przykład, jeśli chcemy zmienić tylko pierwszą literę tekstu na małą, można wykorzystać funkcję "capitalize" w połączeniu z "lower-case" w ten sposób:

```Clojure
(capitalize (lower-case "TEKST NA MAŁE LITERY")) ; zwróci "Tekst na małe litery"
```

## Głębsze wyjaśnienie

W języku Clojure wszystkie napisy są przechowywane jako ciągi znaków, gdzie każdy znak jest jednym elementem ciągu. W celu konwersji tekstu na małe litery, funkcja "lower-case" iteruje przez wszystkie znaki i zmienia je na ich odpowiedniki w postaci małych liter.

W przypadku polskiej transliteracji, konwersja na małe litery może nie być tak prosta, ponieważ wiele liter ma różne odpowiedniki w wersji małych i dużych liter (np. "ś" i "Ś"). W takim przypadku, może być potrzebna dodatkowa logika do sprawdzenia każdego znaku i dopasowania go do odpowiedniego odpowiednika.

## Zobacz również

- Dokumentacja funkcji "lower-case" w języku Clojure: https://clojuredocs.org/clojure.core/lower-case
- Wprowadzenie do języka Clojure: https://www.clojure.org/guides/learn/syntax
- Przykładowe zadania związane z konwersją tekstu na małe litery w języku Clojure: https://www.codewars.com/kata/search/clojure?q=string+to+lower+case