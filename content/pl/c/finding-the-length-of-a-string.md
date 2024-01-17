---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "C: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Znalezienie długości ciągu znaków to proces, w którym programista określa liczbę znaków w danym ciągu. Robi się to z różnych powodów, na przykład w celu wykrywania błędów lub przetwarzania danych.

## Jak to zrobić:
```
// Założyliśmy ciąg znaków "Hello World!"
int length = 0;
for (int i = 0; string[i] != '\0'; i++) {
    length++;
}
printf("Długość ciągu to: %d", length);
```

Wynik: Długość ciągu to: 12

## Wnikliwa analiza:
(1) Proces znajdowania długości ciągu znaków jest starożytny – pisało się go już w języku asembler. (2) Alternatywne metody obejmują użycie funkcji bibliotecznych takich jak `strlen` lub wykorzystanie wskaźnika końca ciągu `\0` w pętli. (3) Implementacja może różnić się w zależności od języka programowania, jednak podstawową ideą jest zliczanie znaków do momentu natrafienia na wskaźnik końca ciągu.

## Zobacz też:
[Strona dokumentacji C](https://pl.cppreference.com/w/c/string/byte/strlen)