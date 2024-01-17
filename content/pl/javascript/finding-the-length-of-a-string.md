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

## Co & Dlaczego?

W programowaniu, "długość" odnosi się do liczby znaków w ciągu znaków. Programiści często muszą obliczyć długość ciągu, aby wykonać różne zadania, takie jak określenie liczby znaków w wyrazie lub tworzenie pętli opartych na długości ciągu.

## Jak to zrobić:

```Javascript
// Przykładowy string
let string = "To jest przykładowy string.";

// Wykorzystanie wbudowanej funkcji 'length' aby obliczyć długość stringa
console.log(string.length);

// Wynik: 27
```

W tym przykładzie, wykorzystujemy wbudowaną funkcję ```length``` w Javascript, aby obliczyć długość naszego stringa. Funkcja ta zwraca liczbę znaków w naszym stringu.

## Głębsza analiza:

### Kontekst historyczny:
W starszych wersjach języka Javascript, nie było wbudowanej funkcji ```length```, a programiści musieli wykorzystywać pętle lub zewnętrzne biblioteki, aby obliczyć długość ciągu.

### Alternatywy:
W niektórych przypadkach, programiści mogą wykorzystać metody takie jak ```split``` lub ```slice```, aby obliczyć długość ciągu. Jednak, wbudowana funkcja ```length``` jest zwykle najprostszym i najszybszym sposobem.

### Szczegóły implementacyjne:
W Javascript, wbudowana funkcja ```length``` jest właściwością obiektu ```String```, a programiści mogą również dostosowywać funkcję ```length``` dla własnych obiektów, aby zwracała liczbę właściwych elementów.

## Zobacz także:
- [Dokumentacja Javascript - wbudowana funkcja 'length'](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/String/length)