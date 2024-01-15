---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Javascript: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Znalezienie długości ciągu znaków jest niezbędnym elementem w wielu programach i aplikacjach internetowych. Pozwala ono na dokładne określenie ilości znaków w danym tekście, co może mieć znaczenie przy walidacji danych lub obliczaniu statystyk.

## Jak to zrobić

```Javascript

// Przykładowy ciąg znaków
let str = "To jest przykładowy tekst.";

// Użycie metody .length
console.log(str.length);
// Output: 27

// Alternatywnie, można użyć pętli for
let count = 0;
for (let i = 0; i < str.length; i++) {
  count++;
}
console.log(count);
// Output: 27
```

## Głębszy przegląd

Długość ciągu znaków jest dość prosta do obliczenia, ponieważ każdy ciąg znaków w języku Javascript ma dostępną metodę .length. Warto jednak zauważyć, że ta metoda liczy również spacje oraz znaki specjalne, więc jeśli potrzebujemy dokładnej ilości liter w tekście, musimy uwzględnić to przy przetwarzaniu danych.

## Zobacz także

- [Metoda length w języku Javascript](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [Wprowadzenie do programowania w języku Javascript](https://kursjs.pl/kurs/intro-js)