---
title:                "Zapisywanie pliku tekstowego"
date:                  2024-01-19
simple_title:         "Zapisywanie pliku tekstowego"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Pisanie pliku tekstowego to zapisywanie danych w formie czytelnej dla człowieka. Programiści robią to, by przechowywać konfigurację, logi czy komunikować się z innymi programami.

## Jak to zrobić:
```Go
package main

import (
    "bufio"
    "fmt"
    "os"
)

func main() {
    file, err := os.Create("przykład.txt")
    if err != nil {
        fmt.Println(err)
        return
    }
    defer file.Close()

    writer := bufio.NewWriter(file)
    _, err = writer.WriteString("Witaj w pliku tekstowym!\n")
    if err != nil {
        fmt.Println(err)
        return
    }

    writer.Flush() // Pamiętaj, by opróżnić bufor!
}
```

Uruchomienie programu stworzy plik `przykład.txt` z zawartością:
```
Witaj w pliku tekstowym!
```

## Więcej informacji:
Pisanie plików tekstowych sięga wczesnych lat informatyki – to jedna z podstawowych umiejętności. Alternatywą może być `ioutil.WriteFile` w Go (deprecated w wersji 1.16), ale `bufio` i `os` to standard. `defer` i `Close` zapobiegają wyciekom zasobów, `Flush` opróżnia bufor, zapewniając, że wszystko jest zapisane.

## Zobacz również:
- Oficjalna dokumentacja Go: https://golang.org/pkg/os/ i https://golang.org/pkg/bufio/
- Go by Example: https://gobyexample.com/writing-files
- Tutorial o czytaniu i pisaniu plików w Go: https://www.devdungeon.com/content/working-files-go
