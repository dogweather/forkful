---
title:                "Pisanie do standardowego błędu"
html_title:           "Arduino: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Pisanie do standardowego błędu (stderr) umożliwia odseparowanie komunikatów o błędach od standardowego wyniku programu (stdout). Programiści używają stderr, aby ułatwić debugowanie i logowanie błędów bez zagracania wyników działania programu.

## Jak to zrobić:
Po prostu używaj `os.Stderr` do pisania błędów. Oto przykładowy kod:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // Standardowy wynik
    fmt.Println("To jest standardowy wynik")

    // Standardowy błąd
    _, err := fmt.Fprintln(os.Stderr, "To jest błąd")
    if err != nil {
        panic(err)
    }
}
```
Gdy uruchomisz ten program, zobaczysz tekst błędu na stderr, a normalny wynik na stdout. Możesz to przetestować, przekierowując wyjścia:

```
go run main.go > wynik.txt 2> bledy.txt
```

## Deep Dive
Historia: stderr jest częścią standardów Uniksa od lat 70. Używanie go to konwencja w wielu systemach operacyjnych i środowiskach programistycznych.

Alternatywy: Możesz użyć logowania lub innych mechanizmów do rejestrowania błędów zamiast stderr. Ale stderr jest prosty i uniwersalny.

Szczegóły implementacyjne: W Go, `os.Stderr` to globalna instancja `*os.File`, która reprezentuje standardowe wyjście błędów. Używa się jej podobnie jak zwykłego pliku do pisania.

## Zobacz także
- Dokumentacja Go dla pakietu `os`: https://golang.org/pkg/os/
- Artykuł o obsłudze błędów w Go: https://blog.golang.org/error-handling-and-go
- Przykłady przekierowywania wyjścia w Uniksie: https://www.gnu.org/software/bash/manual/html_node/Redirections.html