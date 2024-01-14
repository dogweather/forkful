---
title:    "Go: Odczytywanie pliku tekstowego"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Dlaczego

Jeśli piszesz w języku Go lub uczysz się programować w tym języku, na pewno często musisz się spotkać z możliwością odczytu plików tekstowych. Dlatego w tym artykule chciałbym przedstawić Wam prosty sposób na odczyt plików tekstowych w Go, który może pomóc oszczędzić czas i wysiłek.

## Jak to zrobić

Do odczytywania plików tekstowych w Go możemy użyć wbudowanej funkcji `ioutil.ReadFile()` oraz pakietu `fmt`, który pozwala na wyświetlanie wartości zmiennych i tekstu na ekranie.

```Go
package main

import (
    "fmt"
    "io/ioutil"
)

func main() {
    // Otwieramy plik za pomocą funkcji ReadFile() i przypisujemy go do zmiennej data
    data, err := ioutil.ReadFile("plik.txt")
    // Sprawdzamy czy nie wystąpił błąd
    if err != nil {
        fmt.Println("Nie udało się odczytać pliku!")
    }
    // Wyświetlamy zawartość pliku na ekranie
    fmt.Println(string(data))
}
```

Przed uruchomieniem tego kodu pamiętaj, aby utworzyć plik tekstowy o nazwie `plik.txt` w tym samym folderze, w którym znajduje się Twój plik z kodem Go. Możesz również zmienić nazwę pliku w funkcji `ReadFile()` i podać ścieżkę do innej lokalizacji.

### Output

```
To jest przykładowa zawartość pliku.
Znajdziesz tutaj kilka linii tekstu, które będą wyświetlone na ekranie.
Ten tekst możesz dowolnie zmienić lub dodać własne linijki.
```

## Wnikliwe zagłębienie

W powyższym przykładzie korzystamy z funkcji `ioutil.ReadFile()`, która automatycznie otwiera i odczytuje plik tekstowy. Jest to wygodny sposób na szybkie odczytywanie plików, jednak w przypadku dużych plików może to spowodować problemy z wydajnością. W takim przypadku lepiej byłoby korzystać z funkcji `os.Open()` oraz `bufio.Scanner`, co pozwoli na wygodne odczytywanie pliku za pomocą iteracji linijka po linijce.

## Zobacz też

- [Dokumentacja Go: Pakiet `ioutil`](https://golang.org/pkg/io/ioutil/)
- [Dokumentacja Go: Pakiet `bufio`](https://golang.org/pkg/bufio/)
- [Szkolenia programowania w języku Go od "Kursy dla programistów"](https://kursydlaprogramistow.pl/go-online/)