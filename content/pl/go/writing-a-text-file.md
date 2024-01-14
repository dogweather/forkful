---
title:                "Go: Tworzenie pliku tekstowego"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie pliku tekstowego przy użyciu języka programowania Go może być niezbędne w wielu przypadkach, takich jak tworzenie raportów, zapisywania plików konfiguracyjnych lub generowania plików źródłowych dla innych programów. Poniżej przedstawimy kilka prostych przykładów, jak to zrobić przy użyciu Go.

## Jak to zrobić

Poniżej znajduje się przykład kodu w języku Go, który tworzy nowy plik tekstowy o nazwie "sample.txt" i wpisuje do niego tekst. Następnie zapisuje plik i zamyka go.

```Go
package main

import (
    "fmt"
    "os"
)

func main() {

    // utworzenie pliku
    file, err := os.Create("sample.txt")
    if err != nil {
        fmt.Println(err)
    }
    defer file.Close()

    // wpisanie tekstu do pliku
    fmt.Fprintln(file, "Cześć, to jest przykładowy plik tekstowy")

    // zapisanie i zamknięcie pliku
    file.Sync()
}
```

Po uruchomieniu powyższego kodu, w folderze projektu zostanie utworzony plik tekstowy o nazwie "sample.txt", a w nim wpisany będzie nasz przykładowy tekst.

## Deep Dive

Pisanie plików tekstowych w języku Go jest możliwe dzięki modułowi "os", który w swojej funkcjonalności obsługuje operacje związane z systemem operacyjnym, takie jak tworzenie i edycja plików.

W funkcji "main" najpierw używamy funkcji "Create" z modułu "os" by utworzyć nasz plik tekstowy. Następnie, za pomocą funkcji "Fprintln" z modułu "fmt", możemy wpisać tekst do pliku. Na końcu wykorzystujemy funkcję "Sync" do zapisania i zamknięcia pliku.

## Zobacz również

- Dokumentacja języka Go: https://golang.org/doc/
- Przykłady kodu w języku Go: https://gobyexample.com/
- Ucz się programowania w języku Go: https://tour.golang.org/welcome/1