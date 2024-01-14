---
title:    "Go: Pisanie do standardowego błędu"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Dlaczego?

Pisanie do standardowego wyjścia błędu (standard error) jest ważną częścią procesu pisania kodu w języku Go. Jest to szczególnie przydatne w przypadku, gdy chcemy wyświetlić informacje o błędach lub ostrzeżeń podczas wykonywania programu. Ponadto, pisanie do standardowego wyjścia błędu może pomóc nam w debugowaniu naszego kodu.

## Jak to zrobić?

Aby napisać do standardowego wyjścia błędu w języku Go, można użyć funkcji ```Fprintf```, która jest częścią pakietu "fmt". Poniżej znajduje się przykładowy kod pokazujący, jak użyć tej funkcji:

```Go
package main

import (
  "fmt"
  "os"
)

func main() {
  fmt.Fprintf(os.Stderr, "To jest przykładowy błąd") // wyświetla błąd do standardowego wyjścia błędu
}
```

Output:
```
To jest przykładowy błąd
```

## Głębszy wgląd

Pisanie do standardowego wyjścia błędu jest przydatne również w przypadku gdy chcemy wyświetlić więcej informacji o błędzie, takich jak nazwa funkcji, w której wystąpił błąd, czy też linia w kodzie, gdzie wystąpił. Aby uzyskać te informacje, można użyć funkcji ```Caller``` i ```CallerName``` z pakietu "runtime". Przykładowy kod wyglądałby następująco:

```Go
package main

import (
  "fmt"
  "runtime"
)

func main() {
  _, file, line, ok := runtime.Caller(1)
  if ok {
    fmt.Fprintf(os.Stderr, "Błąd wystąpił w pliku %s na linii %d", file, line)
  }
}
```

Output:
```
Błąd wystąpił w pliku main.go na linii 8
```

## Zobacz również

- Dokumentacja pakietu "fmt": https://golang.org/pkg/fmt/
- Dokumentacja pakietu "runtime": https://golang.org/pkg/runtime/
- Przewodnik po języku Go: https://tour.golang.org/