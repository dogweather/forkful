---
title:                "Go: Sprawdzanie, czy istnieje katalog."
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Często podczas pisania programów w języku Go, potrzebujemy sprawdzić czy dany katalog istnieje. W tym artykule dowiecie się dlaczego warto przyjrzeć się temu zagadnieniu oraz jak w prosty sposób można to zrobić.

## Jak To Zrobić

Sprawdzenie czy dany katalog istnieje jest bardzo proste w języku Go. Wystarczy użyć funkcji `os.Stat()` na podanej ścieżce i sprawdzić czy wystąpił błąd lub czy zwrócona wartość jest różna od `nil`. Poniższy przykład pokazuje jak to zrobić:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // Sprawdzenie czy katalog "./test" istnieje
    fileInfo, err := os.Stat("./test")

    if err != nil {
        // Jeśli wystąpił błąd, katalog nie istnieje
        fmt.Println("Katalog nie istnieje")
    } else {
        // Jeśli nie ma błędu, katalog istnieje
        fmt.Println("Katalog istnieje")
        // Dostęp do informacji o katalogu
        fmt.Println("Nazwa: ", fileInfo.Name())
        fmt.Println("Rozmiar: ", fileInfo.Size())
        fmt.Println("Czas modyfikacji: ", fileInfo.ModTime())
        fmt.Println("Czy jest to katalog: ", fileInfo.IsDir())
    }
}
```

### Przykładowy Output:
```
Katalog istnieje 
Nazwa: test 
Rozmiar: 4096 
Czas modyfikacji: 2020-01-01 12:00:00 +0000 UTC 
Czy jest to katalog: true
```

## Deep Dive

Gdybyśmy chcieli sprawdzić czy dany katalog istnieje w systemie plików, należy użyć `os.Stat()`. Ta funkcja zwraca informacje o danym pliku lub katalogu, bądź błąd, jeśli taki wystąpił. Ponadto, możemy również użyć funkcji `os.IsExist()` aby sprawdzić czy dany błąd jest spowodowany już istniejącym plikiem lub katalogiem.

## Zobacz również

- [Dokumentacja języka Go: pakiet os](https://golang.org/pkg/os/)
- [Przykładowy kod: sprawdzanie czy katalog istnieje](https://play.golang.org/p/-QXF4rU3IGH)