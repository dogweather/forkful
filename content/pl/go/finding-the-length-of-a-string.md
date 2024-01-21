---
title:                "Znalezienie długości ciągu znaków"
date:                  2024-01-20T17:47:23.918923-07:00
model:                 gpt-4-1106-preview
simple_title:         "Znalezienie długości ciągu znaków"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Co i dlaczego? Znalezienie długości łańcucha znaków to sprawdzenie, ile znaków zawiera. Programiści robią to, by przykładowo weryfikować dane wejściowe lub zarządzać nimi podczas operacji na tekstach.

## How to:
Jak to zrobić:
```Go
package main

import (
	"fmt"
	"unicode/utf8"
)

func main() {
	sampleText := "Witaj, świecie!"
	fmt.Println("UTF-8 runes:", utf8.RuneCountInString(sampleText)) // Użycie RuneCountInString dla UTF-8
	fmt.Println("Bytes:", len(sampleText))                           // Użycie len dla bajtów
}
```
Output:
```
UTF-8 runes: 15
Bytes: 17
```

## Deep Dive
Szczegółowe spojrzenie: W Go, `len` zwraca liczbę bajtów, ale ze względu na UTF-8, który jest zmienną długością kodowania, to różni się od liczby znaków. Historia Unicode i UTF-8 zaczyna się w latach 80-tych i 90-tych, kiedy potrzeba uniwersalnego kodowania znaków stała się oczywista. W Go, używamy `utf8.RuneCountInString` do prawidłowego policzenia znaków. Alternatywą jest iteracja po łańcuchu runach, ale to bardziej złożone i rzadziej potrzebne.

## See Also
Zobacz także:
- Dokumentacja Go o stringach i runach: https://golang.org/ref/spec#String_types
- Artykuł o kodowaniu UTF-8: https://blog.golang.org/strings