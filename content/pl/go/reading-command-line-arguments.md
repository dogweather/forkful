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

## Dlaczego

Czy kiedykolwiek chciałeś uruchomić swój program z różnymi opcjami, ale nie wiedziałeś jak to zrobić? W takim przypadku, znajomość odczytywania argumentów wiersza poleceń może być bardzo przydatna. Dzięki temu możesz z łatwością dostosować zachowanie programu, bez konieczności zmiany jego kodu.

## Jak to zrobić

Odczytywanie argumentów wiersza poleceń jest łatwe w języku Go dzięki bibliotece "flag". Wystarczy, że importujesz tę bibliotekę, a następnie użyjesz funkcji "flag.XXX" aby odczytać argumenty. Poniżej przedstawiam przykład kodu, który wypisuje wartość podaną jako argument "hello" oraz sprawdza, czy podano opcję "-uppercase".

```
package main

import (
    "flag"
    "fmt"
)

func main() {
    // Zdefiniuj flagi
    helloFlag := flag.String("hello", "", "Wiadomość powitalna")
    upperFlag := flag.Bool("uppercase", false, "Wypisuj tekst wielkimi literami")

    // Wymagane do odczytania flag
    flag.Parse()

    // Wypisz wartość argumentu "hello"
    fmt.Println("Witaj:", *helloFlag)

    // Sprawdź, czy podano opcję "-uppercase"
    if *upperFlag {
        // Wypisz tekst wielkimi literami
        fmt.Println("WITAJ:", *helloFlag)
    }
}
```

### Przykładowe wywołanie z argumentami:

```
go run main.go -hello Jan -uppercase
```

### Przykładowy wynik:

```
Witaj: Jan
WITAJ: JAN
```

## Deep Dive

Biblioteka "flag" jest bardzo pomocna, ale nie jest jedynym sposobem na odczytywanie argumentów wiersza poleceń w Go. Możesz również użyć biblioteki "os", która dostarcza funkcję "os.Args". Ta funkcja zwraca listę argumentów przekazanych do programu, w której pierwszym elementem jest nazwa programu.

Ponadto, warto wiedzieć, że funkcja "flag.Parse()" może rzucić błąd w przypadku, gdy podano niepoprawną lub nieobsługiwaną opcję. Aby uniknąć tego, możesz użyć funkcji "flag.Usage()", która pozwala na dostosowanie wyświetlanego komunikatu błędu lub też wyłączenie wypisywania go całkowicie.

## Zobacz też

- Dokumentacja biblioteki "flag" w języku Go: https://golang.org/pkg/flag/
- Przykładowe wykorzystanie biblioteki "os": https://play.golang.org/p/mMzPQaa22xP
- Przykładowe wykorzystanie funkcji "flag.Usage()": https://play.golang.org/p/ckzJzLJKK-3