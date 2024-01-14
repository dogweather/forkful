---
title:                "Go: Używając wyrażeń regularnych"
simple_title:         "Używając wyrażeń regularnych"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego

Regular expressions (wyrażenia regularne) są nieodłączną częścią wielu języków programowania, w tym także Go. Ich głównym celem jest umożliwienie szybkiego i precyzyjnego wyszukiwania oraz manipulowania tekstem. Dzięki temu są niezbędnym narzędziem dla wielu programistów, którzy chcą zoptymalizować swoje działania i przyspieszyć proces tworzenia oprogramowania.

## Jak to zrobić

W celu wykorzystania regular expressions w Go, należy najpierw zaimportować pakiet "regexp". Następnie, aby stworzyć wyrażenie regularne, możemy użyć funkcji "Compile" z tego pakietu, podając jako argument wzorzec, który chcemy znaleźć. W przykładzie poniżej wyszukujemy wszystkie wystąpienia słowa "hello" w danym tekście:

```Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	text := "Hello world! Hello there!"
	re := regexp.MustCompile("hello")
	fmt.Println(re.FindAllString(text, -1))
}
```
Output: [hello hello]

Funkcja "FindAllString" zwraca tablicę z wszystkimi znalezionymi dopasowaniami, podczas gdy argument "-1" oznacza, że należy znaleźć wszystkie wystąpienia, a nie tylko pierwsze.

## Głębsze zagłębienie

Wyrażenia regularne w Go są oparte na składni wzorca RE2, co oznacza, że niektóre operatory i symbole mogą nie działać tak, jak w innych językach programowania. W celu lepszego zrozumienia działania tych wzorców, warto przejrzeć dokumentację i zapoznać się z różnicami pomiędzy RE2 a innymi standardowymi wyrażeniami regularnymi.

Należy również wspomnieć, że używanie regular expressions może wprowadzić pewne koszty wydajnościowe do kodu, dlatego należy uważnie dobierać wzorce do przetwarzanego tekstu.

## Zobacz również

- Dokumentacja pakietu "regexp" w Go: https://golang.org/pkg/regexp/
- Porównanie wyrażeń regularnych w Go z innymi językami: https://github.com/google/re2/wiki/Syntax