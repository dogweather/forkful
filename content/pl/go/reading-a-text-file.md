---
title:                "Odczytywanie pliku tekstowego"
html_title:           "Go: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Czemu

Jeśli jesteś programistą w Go, zapewne często miałeś lub będziesz miał do czynienia z odczytywaniem plików tekstowych. W tym artykule dowiecie się, jak łatwo i szybko można to zrobić w języku Go.

## Jak

Krok 1: Zaimportuj bibliotekę os
```Go
import "os"
```

Krok 2: Otwórz plik za pomocą funkcji Open
```Go
file, err := os.Open("tekst.txt")
```

Krok 3: Obsłuż ewentualne błędy
```Go
if err != nil {
    panic(err)
}
```

Krok 4: Przeczytaj zawartość pliku za pomocą funkcji Read
```Go
content := make([]byte, 1024)
_, err = file.Read(content)
```

Krok 5: Zamknij plik
```Go
file.Close()
```

Krok 6: Wyświetl zawartość pliku
```Go
fmt.Println(string(content))
```

## Deep Dive

W języku Go funkcja Open z biblioteki os zwraca dwa parametry - obiekt pliku i ewentualny błąd. Zapisujemy je do zmiennych file i err. Jeśli zmienna err jest nierówna nil, oznacza to, że wystąpił błąd i musimy go obsłużyć za pomocą funkcji panic (kończy działanie programu i wyświetla przekazany mu argument).

Funkcja Read zwraca dwa parametry - ilość przeczytanych bajtów (i) oraz ewentualny błąd (err). Warunek _ (podkreślenie) oznacza, że nie chcemy w tym przypadku trzymać wartości i, a jedynie błąd, dlatego go ignorujemy.

## Zobacz też

- Dokumentacja języka Go: https://golang.org/doc/
- Biblioteka os: https://golang.org/pkg/os/
- Przykłady kodu w Go: https://github.com/golang/example