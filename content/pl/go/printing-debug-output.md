---
title:                "Go: Wydrukowanie danych z debugowania"
simple_title:         "Wydrukowanie danych z debugowania"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Debugowanie jest nieodłączną częścią pisania kodu w języku Go. Wypisywanie informacji debuggowania może pomóc programiście zrozumieć, w jaki sposób jego program działa i gdzie ewentualnie pojawiają się błędy. W tym wpisie dowiesz się, dlaczego warto stosować wypisywanie debug output i jak to zrobić w języku Go.

## Jak to zrobić

W języku Go istnieje wiele sposobów na wyświetlanie informacji debuggowania. Najprostszym sposobem jest użycie funkcji `Println()` z pakietu `fmt`. Przykładowy kod wyglądałby następująco:
```
package main

import "fmt"

func main() {
    fmt.Println("Debug output")
}
```

Powyższy kod wypisze w konsoli napis "Debug output". Innym przydatnym sposobem jest użycie funkcji `Printf()` z pakietu `fmt`, która pozwala na formatowanie wyjścia. Przykładowy kod:
```
package main

import "fmt"

func main() {
    name := "John"
    age := 27
    fmt.Printf("My name is %s and I am %d years old.", name, age)
}
```

Wynikiem powyższego kodu będzie wypisanie zdania "My name is John and I am 27 years old." Możliwe jest także wypisanie informacji o błędach używając funkcji `Errorf()`. Przykładowy kod:
```
package main

import "fmt"

func divide(x, y int) (int, error) {
    if y == 0 {
        return 0, fmt.Errorf("cannot divide by zero")
    }
    return x / y, nil
}

func main() {
    result, err := divide(10, 0)
    if err != nil {
        fmt.Printf("An error occurred: %s", err)
    } else {
        fmt.Println(result)
    }
}
```

W powyższym kodzie w przypadku próby podzielenia przez zero, zostanie wyświetlony błąd "An error occurred: cannot divide by zero".

## Deep Dive

W języku Go istnieje także możliwość tworzenia własnych funkcji do wypisywania informacji debuggowania. Przykładowy kod wypisujący informacje o zmiennej i jej wartości wyglądałby następująco:
```
package main

import (
    "fmt"
    "reflect"
)

func debug(v interface{}) {
    fmt.Printf("Value: %v, Type: %T", v, v)
}

func main() {
    name := "Jane"
    age := 30
    debug(name) // wypisze "Value: Jane, Type: string"
    debug(age) // wypisze "Value: 30, Type: int"
}
```

Warto też pamiętać, że wypisywanie informacji debuggowania może mieć znaczny wpływ na wydajność programu. W przypadku wyświetlania zbyt dużej ilości informacji, program może działać znacznie wolniej.

## Zobacz także

- [Funkcje wypisywania informacji debuggowania w języku Go](https://golang.org/pkg/fmt/)
- [Jak debugować w języku Go](https://blog.alexellis.io/golang-debugging-a-love-story/)
- [Przydatne narzędzia do debuggowania w języku Go](https://scene-si.org/2019/08/11/golang-debuggertools/)