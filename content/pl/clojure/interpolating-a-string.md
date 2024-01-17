---
title:                "Interpolacja ciągu znaków"
html_title:           "Clojure: Interpolacja ciągu znaków"
simple_title:         "Interpolacja ciągu znaków"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Interpolacja ciągu znaków w programowaniu to proces wyświetlania zmiennych lub wyrażeń wewnątrz ciągu znaków. Programiści często używają interpolacji ciągu znaków, aby dynamicznie tworzyć tekstowe komunikaty, łączyć różne wartości lub parametry w jednym ciągu lub po prostu poprawić czytelność kodu.

## Jak to zrobić?
```Clojure
;; Użyj znaku '~' przed nawiasami klamrowymi, aby wstawić wyrażenie w odpowiedni miejscu ciągu
(str "Witaj, ~" name "!")


;; Interpolacja liczby zmiennoprzecinkowej w celu uzyskania dokładności do 2 miejsc po przecinku
(def tax-rate 0.235)
(str "Stawka podatkowa wynosi ~" (format "%.2f" tax-rate) "%")


;; Łączenie wielu zmiennych w jednym ciągu
(def first-name "Anna")
(def last-name "Kowalska")
(str "Twoje imię to ~" first-name ", a nazwisko to ~" last-name)
```

## Głębsza analiza
Interpolacja znaków ma swoje korzenie w języku Perl i została przyjęta przez innych językach programowania, takich jak Python, Ruby i oczywiście Clojure. Alternatywami do interpolacji znaku są m.in. konkatenacja ciągów za pomocą funkcji ```(str)``` lub operatora ```+```, jednak interpolacja jest bardziej czytelna i elastyczna.

## Zobacz także
• Dokumentacja Clojure dotycząca interpolacji ciągu znaków: https://clojure.org/guides/learn/syntax#_string_interpolation