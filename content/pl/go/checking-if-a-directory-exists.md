---
title:                "Sprawdzanie, czy katalog istnieje"
date:                  2024-01-20T14:56:57.726025-07:00
html_title:           "Fish Shell: Sprawdzanie, czy katalog istnieje"
simple_title:         "Sprawdzanie, czy katalog istnieje"

category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)

Sprawdzanie, czy katalog istnieje, to proces weryfikowania obecności folderu w systemie plików. Programiści to robią, żeby uniknąć błędów przy próbie dostępu lub zapisu, i żeby wiedzieć, kiedy trzeba utworzyć nowy katalog.

## How to: (Jak to zrobić:)

W Go można użyć `os.Stat()` i sprawdzić error zwrócony przez funkcję:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    dir := "/path/to/directory"

    if _, err := os.Stat(dir); os.IsNotExist(err) {
        fmt.Printf("Directory %s does not exist.\n", dir)
    } else {
        fmt.Printf("Directory %s exists.\n", dir)
    }
}
```

Uruchomienie kodu z istniejącym katalogiem:

```Go
Directory /path/to/directory exists.
```

Uruchomienie kodu z nieistniejącym katalogiem:

```Go
Directory /path/to/directory does not exist.
```

## Deep Dive: (Dogłębna analiza)

Metoda `os.Stat()` zwraca informacje o pliku/katalogu. Jeśli plik/katalog nie istnieje, error jest typu `*PathError`. `os.IsNotExist(err)` rozpoznaje takie błędy.

Historycznie, sprawdzanie istnienia katalogu było różne w zależności od systemu operacyjnego i języka. W Go stawia się na prostotę i spójność międzyplatformową.

Alternatywnie, `os.IsNotExist()` może być użyty z `os.Open()` – jeśli nie możesz otworzyć pliku/katalogu, prawdopodobnie nie istnieje.

Ważny szczegół: `os.Stat()` może zwrócić inne błędy, nie tylko fakt, że ścieżka nie istnieje. Zawsze warto sprawdzić dokładny rodzaj błędu.

## See Also: (Zobacz także)

- Dokumentacja Go na temat pakietu `os`: https://pkg.go.dev/os
- Więcej o obsłudze błędów w Go: https://blog.golang.org/error-handling-and-go
- Artykuł na temat systemu plików i operacji na plikach w Go: https://golangbot.com/read-files/
- Wzorce projektowe dla systemów plików w Go: https://www.oreilly.com/library/view/go-design-patterns/9781788390552/ch04s04.html
