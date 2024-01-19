---
title:                "Analiza składniowa daty z ciągu znaków"
html_title:           "Clojure: Analiza składniowa daty z ciągu znaków"
simple_title:         "Analiza składniowa daty z ciągu znaków"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Przetwarzanie daty z napisu to technika, która pozwala nam zmienić datę zapisaną jako ciąg znaków na format daty, który można potem używać w operacjach programistycznych. Programiści to robią, by móc używać dat w bardziej złożonych operacjach, takich jak porównywanie, sortowanie czy różne obliczenia.

## Jak to Zrobić:
Go oferuje wbudowaną funkcję `time.Parse`, która umożliwia parsowanie dat z łańcucha znaków. 
```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    dataString := "2022-05-26"
    data, _ := time.Parse("2006-01-02", dataString)
    fmt.Println(data)
}
```
Powinieneś zobaczyć coś takiego po uruchomieniu powyższego kodu:
```
2022-05-26 00:00:00 +0000 UTC
```
## Dogłębne Zrozumienie
Pierwotnie, nie istniał żaden standard dla reprezentacji dat jako ciągów znaków, co prowadziło do wielu problemów (np. 12/01/22 to po polsku 12 stycznia, a po angielsku 1 grudnia). W 1988 roku ISO opracowało standard ISO 8601, który umożliwia jednoznaczną reprezentację daty zapisanej jako ciąg znaków.

Alternatywą dla funkcji `time.Parse` jest stosowanie formatu z unix, który jest znacznie mniej czytelny dla ludzi, ale szybszy dla maszyn. Na przykład: `1514764800` reprezentuje "2018-01-01T00:00:00Z".

Szczegół implementacji: `time.Parse` zasługuje na wzmiankę, że format, którego używasz jako wzorca parsowania, to nie wzorzec jakiegokolwiek formatu zapisu daty, ale to musi być `"Mon Jan 2 15:04:05 MST 2006"` napisane naszymi wybranymi danymi. 

## Zobacz Również
1. Dokumentacja Golang na temat pakietu czasu [tutaj](https://golang.org/pkg/time/).
2. Przykłady kodu [tutaj](https://golangcode.com/convert-string-to-datetime/). 
3. Historia standardu ISO 8601 [tutaj](https://en.wikipedia.org/wiki/ISO_8601).