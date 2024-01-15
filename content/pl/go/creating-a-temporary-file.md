---
title:                "Tworzenie tymczasowego pliku"
html_title:           "Go: Tworzenie tymczasowego pliku"
simple_title:         "Tworzenie tymczasowego pliku"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie tymczasowych plików jest ważnym aspektem programowania, ponieważ często potrzebujemy przechowywać tymczasowe dane lub kod w celu weryfikacji funkcjonalności lub rozwiązywania problemów. W tym artykule dowiesz się, jak w prosty sposób wykorzystać Go do tworzenia tymczasowych plików.

## Jak to zrobić

Aby utworzyć tymczasowy plik w Go, wystarczy użyć funkcji `TempFile` z biblioteki `io/ioutil` i przypisać ją do zmiennej. Następnie możemy wykorzystać tę zmienną do wykonywania operacji na pliku, takich jak pisanie lub odczyt.

```Go
package main

import (
    "fmt"
    "io/ioutil"
)

func main() {
    // Tworzenie tymczasowego pliku w bieżącym katalogu
    tempFile, err := ioutil.TempFile(".", "temp")

    // Sprawdzanie, czy wystąpił błąd
    if err != nil {
        fmt.Println(err)
    }

    // Zwrócenie nazwy pliku tymczasowego
    fmt.Println("Nazwa pliku tymczasowego:", tempFile.Name())

    // Zapisanie tekstu do pliku
    tempFile.WriteString("Przykładowy tekst")

    // Odczytanie tekstu z pliku
    data, err := ioutil.ReadFile(tempFile.Name())

    // Sprawdzanie, czy wystąpił błąd
    if err != nil {
        fmt.Println(err)
    }

    // Wyświetlenie odczytanego tekstu
    fmt.Println("Zawartość pliku:", string(data))
}
```

Po uruchomieniu powyższego kodu, powinniśmy zobaczyć wyjście:

```
Nazwa pliku tymczasowego: ./temp921141634
Zawartość pliku: Przykładowy tekst
```

## Deep Dive

Funkcja `TempFile` tworzy tymczasowy plik w bieżącym katalogu z nazwą rozpoczynającą się od prefiksu podanego w drugim parametrze. Domyślnym miejscem, w którym tworzony jest plik, jest katalog systemowy dla plików tymczasowych, który jest różny dla każdego systemu operacyjnego.

Oprócz funkcji `TempFile`, istnieje również funkcja `TempDir`, która pozwala na utworzenie tymczasowego katalogu w podanej ścieżce. Użyteczne jest także wykorzystanie funkcji `Remove` z biblioteki `os`, która pozwala na usunięcie tymczasowego pliku lub katalogu po zakończeniu pracy z nim.

## Zobacz także

- [Dokumentacja biblioteki `io/ioutil` w języku Go](https://golang.org/pkg/io/ioutil/)
- [Poradnik na temat obsługi plików w Go](https://gobyexample.com/reading-files)
- [Artykuł o obsłudze błędów w języku Go](https://www.calhoun.io/when-nil-is-not-null-in-go/)