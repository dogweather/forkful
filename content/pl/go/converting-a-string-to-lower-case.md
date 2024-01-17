---
title:                "Konwertowanie ciągu znaków na małe litery"
html_title:           "Go: Konwertowanie ciągu znaków na małe litery"
simple_title:         "Konwertowanie ciągu znaków na małe litery"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Konwersja ciągu znaków na małe litery jest działaniem polegającym na zmniejszeniu wielkości liter w danym tekście. Programiści często wykonują tę czynność, ponieważ chcą zmniejszyć możliwość błędów, które mogą wyniknąć z różnej wielkości liter w tekście.

## Jak to zrobić:
```Go
str := "TEKST Z DUŻYCH LITER"
fmt.Println(strings.ToLower(str))
```
Wynik: tekst z dużych liter

## Głębszy Wgląd:
1. Kontekst historyczny:
Konwersja na małe litery jest powszechnie stosowana w programowaniu od dawna, zwłaszcza w językach, które nie są wrażliwe na wielkość liter.
2. Alternatywy:
Alternatywą dla użycia funkcji `ToLower()` jest ręczne zmniejszanie wielkości liter za pomocą pętli i warunków.
3. Szczegóły implementacji:
W Go, funkcja `ToLower()` jest częścią pakietu `strings` i używa tablicy mapującej znaki na ich małe odpowiedniki.

## Zobacz również:
- [Dokumentacja Go na temat funkcji ToLower()](https://golang.org/pkg/strings/#ToLower)
- [Inne sposoby konwersji ciągów znaków w Go](https://play.golang.org/p/xw54RIc7vQE)