---
title:                "Go: Sprawdzanie istnienia katalogu"
simple_title:         "Sprawdzanie istnienia katalogu"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Niektórzy z nas chcieliby rozpocząć działalność w Go, ale nie wiedzą, gdzie zacząć. Jednym z najważniejszych aspektów programowania jest umiejętność manipulacji plikami i katalogami. Dlatego też kluczowa jest umiejętność sprawdzania, czy dany katalog istnieje. W tym artykule pokażemy w jaki sposób można to zrobić w języku Go.

## Jak to zrobić

Aby sprawdzić, czy dany katalog istnieje, musimy skorzystać z pakietu `os`. Znajdziemy w nim funkcję `Stat()`, która pozwala nam na sprawdzenie metadanych pliku lub katalogu. Przykładowy kod wykorzystujący tę funkcję wyglądałby następująco:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // Sprawdzamy, czy katalog "projekty" istnieje
    if _, err := os.Stat("projekty"); os.IsNotExist(err) {
        fmt.Println("Katalog nie istnieje.")
    } else {
        fmt.Println("Katalog istnieje.")
    }
}
```

W tym przykładzie najpierw importujemy potrzebne pakiety - `fmt` oraz `os`. Następnie wykorzystujemy funkcję `Stat()` wraz z nazwą katalogu, którego istnienie chcemy sprawdzić. Jeśli funkcja zwraca błąd i ten błąd wskazuje na to, że katalog nie istnieje, wówczas wypisujemy odpowiedni komunikat. W przeciwnym wypadku oznacza to, że katalog istnieje i także możemy to wyświetlić na ekranie.

## Dogłębna analiza

Sprawdzanie, czy katalog istnieje, może być ważne na różnych etapach naszej pracy. Na przykład, przy tworzeniu nowych plików czy katalogów chcielibyśmy najpierw sprawdzić, czy nie istnieją już o podanej nazwie. Jeśli tak, to unikniemy błędów podczas próby nadania takiej samej nazwy.

Innym przypadkiem jest praca z wieloma katalogami i plikami, gdzie możliwe jest, że jakiś katalog nie zostanie utworzony lub zostanie usunięty przez użytkownika. Dzięki sprawdzeniu na początku, czy dany katalog istnieje, możemy zapobiec błędom w dalszych operacjach na plikach.

## Zobacz również

- [Dokumentacja pakietu "os" w języku Go](https://golang.org/pkg/os/)
- [Porównanie plików i katalogów w języku Go](https://www.calhoun.io/comparing-files-and-folders-in-go/)