---
title:    "Go: Wydrukowanie danych debugowania"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/go/printing-debug-output.md"
---

{{< edit_this_page >}}

# Dlaczego warto wyświetlać wyjście debugowania w języku Go?

Wyświetlanie outputu debugowania jest ważnym narzędziem w procesie pisania kodu. Nie tylko pozwala to na szybkie odnajdywanie błędów, ale również pozwala na zrozumienie działania programu i jego logiki. W języku Go jest to jeszcze prostsze dzięki wbudowanej funkcji do wyświetlania outputu.

## Jak wyświetlać wyjście debugowania w języku Go?

Aby wyświetlać wyjście debugowania w języku Go, możemy skorzystać z funkcji `Println` z pakietu `fmt`. W poniższym przykładzie możemy zobaczyć jak wyświetlić wartość zmiennej `x`:

```Go
package main

import "fmt"

func main() {
  x := 10
  fmt.Println(x)
}
```

Output: `10`

Możemy również wyświetlać output w formie formatowanej za pomocą funkcji `Printf` z tego samego pakietu. Przykład poniżej pokazuje wyświetlenie liczby z uwzględnieniem dwóch miejsc po przecinku:

```Go
package main

import "fmt"

func main() {
  x := 3.14159
  fmt.Printf("Liczba pi to: %.2f", x)
}
```

Output: `Liczba pi to: 3.14`

## Deep Dive: Vertically

W języku Go istnieje możliwość wyświetlenia wyjścia debugowania w pionie przy użyciu funkcji `Println`. Wystarczy użyć wielu argumentów, oddzielając je przecinkami:

```Go
package main

import "fmt"

func main() {
  x := 10
  y := "Go"
  fmt.Println("Wartość x:", x, "Wartość y:", y)
}
```

Output: `Wartość x: 10 Wartość y: Go`

Możemy również wyświetlać wyjście w pionie przy użyciu funkcji `Printf` i specjalnego znaku nowej linii `\n`:

```Go
package main

import "fmt"

func main() {
  x := 10
  y := "Go"
  fmt.Printf("Wartość x: %d\nWartość y: %s", x, y)
}
```

Output: 
```
Wartość x: 10
Wartość y: Go
```

Wyświetlanie wyjścia w pionie może być szczególnie przydatne przy wyświetlaniu informacji z wielu zmiennych lub w celu czytelniejszego formatowania outputu.

## Zobacz też

- [Dokumentacja języka Go](https://golang.org/doc/)
- [Podstawowe operacje na tekstach w języku Go](https://golang.org/pkg/strings/)
- [Tutorial na temat używania funkcji fmt w języku Go](https://gobyexample.com/string-formatting)