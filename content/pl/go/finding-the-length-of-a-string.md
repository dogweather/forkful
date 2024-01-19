---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Arduino: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Długość stringa to liczba znaków. Programiści potrzebują jej do kontrolowania danych wejściowych lub manipulowania nimi.

## Jak to zrobić:

Aby znaleźć długość stringa w Go, używamy funkcji len(). Oto przykład:

```Go
package main
import "fmt"

func main() {
    str := "Witaj, Świecie!"
    fmt.Println(len(str))
}
```

Na wyjściu zobaczysz wartość 16 - to długość naszego stringa.

## Zagłębianie się

- Kontekst historyczny: Język Go from the beginning miał wbudowaną funkcję len() do obsługi różnych typów danych, w tym stringów.

- Alternatywy: Pamiętaj o rune. W przypadku stringów UTF-8 len() zwraca liczbę bajtów, a nie znaków. Jeśli potrzebujesz liczby znaków, użyj 'range' i 'rune'.

```Go
package main
import "fmt"

func main() {
    str := "Dzień dobry"
    runes := 0
    for range str {
        runes++
    }
    fmt.Println(runes)
}
```

- Szczegóły implementacji: Funkcja len() jest częścią języka Go, a nie pakietu standardowego. Oznacza to, że może działać na tablicach statycznych, segmentach, mapach, kanałach i stringach bez konieczności konwersji.

## Zobacz także:

- Dokumentacja Go na temat len(): https://golang.org/pkg/builtin/#len

- Wejście na blogu dotyczące różnicy między bajtami a runami: https://blog.golang.org/strings