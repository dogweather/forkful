---
title:    "Go: Konwersja ciągu znaków na małe litery"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Dlaczego

Podczas pisania kodu w języku Go często konieczne jest przekształcenie tekstu w postaci wielkich liter na małe litery. To częsta operacja, która może przydać się w wielu różnych sytuacjach, dlatego warto poznać sposób jej wykonania w języku Go.

## Jak To Zrobić

Istnieją różne metody konwertowania tekstu na małe litery w języku Go, ale najbardziej popularną jest użycie funkcji strings.ToLower(). Spójrzmy na przykład:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "Witaj Świecie"
    fmt.Println(strings.ToLower(str))
}
```

W powyższym kodzie, zmienna "str" zawiera tekst, który chcemy przekonwertować na małe litery. Następnie, wywołujemy funkcję strings.ToLower(), która zwraca przekonwertowany tekst w postaci małych liter. Wynik tego programu będzie wyglądał następująco:

```
witaj świecie
```

Można również wykorzystać pętlę for do iteracji po każdym znaku w tekście i zmienić go na małą literę. Jednak funkcja strings.ToLower() jest bardziej wydajna i zalecana do tego typu operacji.

## Głęboki Zanurzenie

W języku Go, konwersja na małe litery jest wykonywana zgodnie z aktualnym ustawieniem lokalizacji (locale). Dzięki temu, operacja ta będzie odpowiadała specyficznym wymaganiom dla każdego języka. Można również skorzystać z innych funkcji, takich jak strings.ToUpper(), aby przekonwertować tekst na duże litery lub strings.Title() do konwersji pierwszych liter w każdym wyrazie na dużą.

## Zobacz również
- Dokumentacja języka Go: https://golang.org/doc/
- Porównanie funkcji konwertujących tekst w języku Go: https://golang.org/pkg/strings/
- Wskazówki i porady dotyczące programowania w języku Go: https://blog.golang.org/