---
title:                "Wyszukiwanie i zastępowanie tekstu"
html_title:           "Javascript: Wyszukiwanie i zastępowanie tekstu"
simple_title:         "Wyszukiwanie i zastępowanie tekstu"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Co to i dlaczego?
Wyszukiwanie i zastępowanie tekstu to proste, acz potężne narzędzie, które umożliwia odnajdywanie określonej frazy i zmienianie jej na inną. Programiści robią to, aby modyfikować istniejący kod lub prościej przetwarzać dane.

## Jak zrobić:
W Go, aby wyszukać i zastąpić tekst, używamy `strings.Replace()`. Oto prosty przykład:

```Go
package main
import "strings"
import "fmt"
func main() {
   str := "Dzień dobry, świecie!"
   str = strings.Replace(str, "świecie", "Poland", -1)
   fmt.Println(str)
}
```

Przykładowy wynik:

```Go
Dzień dobry, Poland!
```

Możemy użyć trzeciego parametru w `Replace()`, aby kontrolować liczbę zastąpień. 

## Głębsza analiza
Historia wyszukiwania i zastępowania tekstu sięga czasy edytora vi, który jako pierwszy udostępnił te funkcje. W Go, `strings.Replace()` to niskopoziomowa funkcja, która działa skanując łańcuch od lewej do prawej. Co do zastępowania, w Go mamy również `ReplaceAll()` który działa podobnie jak `Replace()` z -1 jako trzecim argumentem.

## Zobacz również
Możesz odwiedzić oficjalna [dokumentację Go dla pakietu strings](https://golang.org/pkg/strings/) aby nauczyć się więcej o innych funkcjach dostępnych w pakiecie `strings`. Projekt Go ma również [kanał na YouTube](https://www.youtube.com/user/gocoding) gdzie regularnie udostępniają nowe filmy na temat różnych aspektów języka.