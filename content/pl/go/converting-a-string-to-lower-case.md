---
title:                "Konwertowanie łańcucha znaków na małe litery"
html_title:           "Go: Konwertowanie łańcucha znaków na małe litery"
simple_title:         "Konwertowanie łańcucha znaków na małe litery"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Kodowanie w języku Go wymaga umiejętności wykonywania różnych operacji, takich jak konwertowanie danych. Jedną z często spotykanych czynności jest konwertowanie tekstu na małe litery. W tym artykule dowiesz się, jak to zrobić w prosty i efektywny sposób.

## Jak To Zrobić

Konwersja tekstu na małe litery w Go jest bardzo prosta dzięki funkcji strings.ToLower(). Wystarczy podać jako argument instrukcję przekonwertowanego tekstu:

```Go 
lowerCaseText := strings.ToLower("PRZYKŁADOWY TEKST")
fmt.Println(lowerCaseText)
```
Wyjściem z powyższego kodu będzie "przykładowy tekst".

W przypadku gdy chcemy zamienić tekst na małe litery tylko w konkretnym zakresie, możemy użyć funkcji strings.ToLowerSpecial() wraz z funkcją RangeTable. Przykładowy kod może wyglądać następująco:

```Go
table := unicode.RangeTable{LatinOffset16, GreekRanges, CyrillicRanges}
lowerCaseText := strings.ToLowerSpecial(table, "ŻÓŁĆ")
fmt.Println(lowerCaseText)
```

Wyjściem z powyższego kodu będzie "żółć".

## Deep Dive

W języku Go konwersja tekstu na małe litery jest możliwa dzięki funkcji strings.ToLower(), która bazuje na standardzie Unicode. Funkcja ta może również obsługiwać tekst w różnych językach, dzięki czemu konwersja jest dokładna i precyzyjna.

Ponadto, funkcja strings.ToLowerSpecial() pozwala na specyfikowanie konkretnej tabeli znaków, co jest przydatne w przypadku pracy z różnymi alfabetami.

## Zobacz również

- Dokumentacja języka Go dotycząca funkcji strings.ToLower(): https://golang.org/pkg/strings/#ToLower
- Przykładowe kody i wyjaśnienia z konwersją tekstu na małe litery w Go: https://www.golangprograms.com/how-to-convert-a-string-to-lowercase-in-golang.html