---
title:                "Konwersja ciągu znaków na małe litery"
html_title:           "Fish Shell: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Zmiana tekstu na małe litery to fundamentalna operacja w programowaniu, polegająca na konwersji wszystkich liter w ciągu na małe litery. Programiści robią to, gdy chcą unormować danych do jednego formatu, zwłaszcza w przypadku porównania i sortowania.

## Jak to zrobić: 

Przyjrzyjmy się składni konwersji tekstu na małe litery w Go. 

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	var myString string = "TechInputLabel"
	fmt.Println(strings.ToLower(myString))
}
```

Po wykonaniu powyższego kodu, wyjście będzie wyglądało tak:

```Go
techinputlabel
```

## Głębsze zrozumienie 

Konwersja tekstu na małe litery jest jedną z podstawowych operacji wprowadzanych przez języki programowania od początków ich istnienia. Istnieją różne metody konwersji tekstu na małe litery zależnie od technologii, ale wszystkie mają na celu zapewnienie jednolitości danych.

W Go, standardowa biblioteka oferuje funkcję `ToLower`, która jest częścią pakietu `strings`. Używa ona mapowania Unicode do konwersji znaków. Jest to najczęściej używany sposób konwersji tekstu na małe litery w Go.

Alternatywą dla tej techniki może być napisanie własnej funkcji, która iteruje po każdym znaku w ciągu i konwertuje go na małą literę, ale to jest mniej wydajne i zalecane tylko w sytuacjach, gdy pakiet `strings` nie jest dostępny.

## Zobacz również

Aby dowiedzieć się więcej na temat manipulacji ciągów w Go, oto kilka obszernych zasobów:

1. Oficjalna dokumentacja języka Go, pakietu `strings` (https://pkg.go.dev/strings)

2. Artykuł o manipulacji ciągów w Go na stronie DigitalOcean (https://www.digitalocean.com/community/tutorials/how-to-work-with-strings-in-go)