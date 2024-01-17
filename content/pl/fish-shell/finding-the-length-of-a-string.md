---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Fish Shell: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Czym jest i dlaczego?
Znalezienie długości ciągu znaków to częsta czynność w programowaniu. Polega ona na określeniu liczby znaków w danym tekście. Najczęściej wykonuje się ją w celu sprawdzenia poprawności danych lub do przeprowadzenia operacji na tekstach. 

## Jak to zrobić:
Przykładowy kod w języku Fish Shell, który wyświetli długość ciągu znaków:
```
Fish Shell - Twój tekst | wc -c
```
Przykładowy wyjście:
```
12
```
W tym przypadku "Twój tekst" to tekst, którego długość chcemy sprawdzić.

## Deep Dive:
Znajdowanie długości ciągu znaków jest bardzo popularne w programowaniu, ponieważ często jest niezbędne do przeprowadzenia różnych operacji. W przeszłości stosowano różne metody na znalezienie długości ciągu znaków, takie jak obliczanie za pomocą iteratora lub pętli. Jednak obecnie większość języków programowania posiada wbudowane funkcje lub metody, które wykonują tę czynność.

Inną możliwością jest użycie polecania `tr -d '\0' | wc -c`, które usuwa znaki nowej linii i spacji z tekstu, a następnie zlicza pozostałe znaki, dając w efekcie długość ciągu znaków.

## Zobacz też:
- [Artykuł na blogu o obliczaniu długości ciągu znaków w innym języku programowania](https://sampleblog.com/obliczanie-dlugosci-ciagu-znakow)
- [Dokumentacja Fish Shell](https://fishshell.com/docs/current/)
- [Github - kod źródłowy Fish Shell](https://github.com/fish-shell/fish-shell)