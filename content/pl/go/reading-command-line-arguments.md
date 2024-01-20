---
title:                "Czytanie argumentów linii poleceń"
html_title:           "Bash: Czytanie argumentów linii poleceń"
simple_title:         "Czytanie argumentów linii poleceń"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Czytanie argumentów z linii poleceń to sposób, w jaki program komputerowy odbiera dane wejściowe podane przez użytkownika w momencie uruchomienia. Robimy to, aby umożliwić naszym programom działanie w bardziej elastyczny i konfigurowalny sposób.

## Jak to zrobić:

Oto prosty przykład, jak czytać argumenty z linii poleceń w języku Go:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	argsWithProg := os.Args
	argsWithoutProg := os.Args[1:]

	arg := os.Args[3]

	fmt.Println(argsWithProg)
	fmt.Println(argsWithoutProg)
	fmt.Println(arg)
}
```

Podczas uruchamiania tego programu z argumentami, np.: `./main arg1 arg2 arg3`, wynikiem będzie:

```bash
[./program arg1 arg2 arg3]
[arg1 arg2 arg3]
arg3
```

## Głębsze zrozumienie:

Czytanie argumentów z linii poleceń jest z nami od dawna. Wiele starszych języków programowania, takich jak C i Perl, również dostarcza metod do czytania argumentów z linii poleceń.

Jednym z alternatywnych podejść jest użycie `flag` package dostarczonego przez Go. Pozwala on na bardziej wyspecjalizowane odczytywanie flag i argumentów.

Szczegółem implementacji godnym uwagi jest to, że `os.Args` dostarcza nam pełną listę argumentów, włącznie z nazwą programu `(os.Args[0])`. Stąd często widzisz `os.Args[1:]`, co pomija nazwę programu.

## Zobacz również:

Dla lepszego zrozumienia problemu, sugerowane są następujące źródła:
- Dokumentacja Go na temat pakietu `os`: https://golang.org/pkg/os/
- Dokumentacja Go na temat pakietu `flag`: https://golang.org/pkg/flag/ 
- Przykładowy tutorial na temat argumentów wiersza polecenia w Go: https://gobyexample.com/command-line-arguments