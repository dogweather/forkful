---
title:                "Odczytywanie argumentów wiersza poleceń"
html_title:           "Go: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Czy kiedykolwiek widziałeś w terminalu, że programista wpisuje coś po włączeniu programu? To właśnie jest czytanie argumentów wiersza poleceń! Programiści robią to aby ustawić pewne opcje lub przekazać dane pomiędzy programami.

## Jak:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	args := os.Args[1:]
	fmt.Println("Hello " + args[0] + "!")
}
```

Wejdź w terminal i wywołaj ten program, podając swoje imię jako argument:

```
$ Go run program.go Kasia
Hello Kasia!
```

## Deep Dive:

Czytanie argumentów wiersza poleceń jest popularną techniką, która pozwala programistom na dostosowywanie swoich programów do różnych zastosowań. Alternatywą dla tego jest ustawianie stałych wartości w samym kodzie programu, co czyni go mniej uniwersalnym. Implementacja czytania argumentów zależy od języka programowania, jednak w Go jest to proste dzięki bibliotece `os`. Wcześniej, w czasach konsolowych interfejsów użytkownika, czytanie argumentów wiersza poleceń było jedynym sposobem na interakcję z programem.

## Zobacz też:

- Oficjalna dokumentacja Go na temat czytania argumentów wiersza poleceń: https://golang.org/pkg/os/#Args
- Przydatny artykuł na temat czytania argumentów wiersza poleceń w Go: https://flaviocopes.com/go-program-command-line-arguments/