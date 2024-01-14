---
title:                "Go: Zmiana ciągu znaków na małe litery"
simple_title:         "Zmiana ciągu znaków na małe litery"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego ktoś chciałby przekonwertować ciąg znaków na małe litery? Jest kilka powodów dla których moglibyśmy to zrobić. Przede wszystkim, może to być potrzebne w celu ujednolicenia danych wejściowych, tak aby niezależnie od przypadków liter w ciągu, był on traktowany jako to samo słowo. Może być również przydatne przy porównywaniu i sortowaniu tekstów.

## Jak to zrobić

Aby przekonwertować ciąg znaków na małe litery w języku Go, możemy użyć funkcji `strings.ToLower()` lub `strings.ToLowerSpecial()` w połączeniu z pakietem `strings`. Oto przykłady kodu, który pokazują jak to zrobić:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "HELLO WORLD"
	fmt.Println(strings.ToLower(str)) //output: hello world
	fmt.Println(strings.ToLowerSpecial(unicode.TurkishCase, "Istanbul")) //output: istanbul
}
```

Jak widać, obie funkcje zwracają przekonwertowany ciąg znaków, w którym wszystkie litery są małe. W przypadku użycia `ToLowerSpecial()`, możemy określić język i styl pisania, aby uzyskać odpowiednią konwersję.

## Pod lupą

Przeglądając dokładniej funkcje `ToLower()` i `ToLowerSpecial()`, możemy zauważyć, że obie używają metody `unicode.SimpleFold()` do przeprowadzenia konwersji. Metoda ta mapuje bieżące znaki na ich składnik, a następnie używa funkcji `unicode.ToLower()` dla każdego z tych składników.

Funkcja `ToLowerSpecial()` używa metod `unicode.SimpleFold()` oraz `unicode.SpecialCase.ToLower()` aby obsłużyć różne przypadki specjalne w różnych językach.

## Zobacz również

- Dokumentacja Go dla pakietu `strings`: https://golang.org/pkg/strings/
- Dokumentacja Go dla pakietu `unicode`: https://golang.org/pkg/unicode/
- Przykładowy kod na konwersję ciągu znaków na małe litery: https://play.golang.org/p/Q2DeuYNL051