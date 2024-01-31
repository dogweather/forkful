---
title:                "Odczytywanie argumentów linii poleceń"
date:                  2024-01-20T17:56:26.887981-07:00
model:                 gpt-4-1106-preview
simple_title:         "Odczytywanie argumentów linii poleceń"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Czytanie argumentów linii poleceń to sposób, by program Go mógł otrzymywać dane wejściowe od użytkownika bez interfejsu użytkownika. Programiści wykorzystują to do tworzenia elastycznych narzędzi, które pracują z różnymi danymi bez potrzeby zmian w kodzie.

## How to: (Jak to zrobić:)
```go
// Przykład: main.go
package main

import (
	"fmt"
	"os"
)

func main() {
	args := os.Args[1:] // Ignoruje nazwę programu (args[0])
	for i, arg := range args {
		fmt.Printf("Argument %d: %s\n", i+1, arg)
	}
}
```
Uruchomienie w terminalu:
```
$ go run main.go these are arguments
```
Wyjście:
```
Argument 1: these
Argument 2: are
Argument 3: arguments
```

## Deep Dive (Dogłębna analiza)
Go, od pierwszych wersji, wyposażony jest w `os` package do interakcji z systemem operacyjnym. Argumenty linii poleceń są dostępne poprzez `os.Args`, tablicę stringów. Element `os.Args[0]` to ścieżka do uruchomionego programu, dlatego pomijany jest podczas dostępu do argumentów. Alternatywy jak `flag` albo `cobra` służą do bardziej złożonych scenariuszy z flagami czy opcjami. Implementacja jest prosta, ale umożliwia pisanie skryptów i narzędzi zdolnych do przetwarzania danych wejściowych dynamicznie.

## See Also (Zobacz również)
- Oficjalna dokumentacja Go dla `os` package: https://pkg.go.dev/os
- Pakiet `flag` do zarządzania flagami komend: https://pkg.go.dev/flag
- Cobra, framework do tworzenia potężnych aplikacji komendowych w Go: https://github.com/spf13/cobra
