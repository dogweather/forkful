---
title:    "Go: Tworzenie pliku tekstowego"
keywords: ["Go"]
---

{{< edit_this_page >}}

# Dlaczego warto pisać pliki tekstowe w języku Go?

Istnieje wiele powodów, dla których warto używać języka Go do pisania plików tekstowych. Po pierwsze, jest to język prosty i czytelny, co ułatwia pracę z dużymi zestawami danych. Ponadto, język ten posiada bogatą bibliotekę standardową, która oferuje wiele narzędzi do manipulacji i przetwarzania plików tekstowych.

# Jak to zrobić?

Na początek należy zaimportować pakiet "os", który zawiera funkcje do obsługi systemu operacyjnego. Następnie, należy utworzyć i otworzyć plik tekstowy za pomocą funkcji "Open" podając nazwę pliku i tryb dostępu. W poniższym przykładzie, wpisujemy tekst do pliku "plik.txt" w formacie "tekst:liczba" i wyświetlamy zawartość pliku na ekranie.

```Go
package main

import "os"

func main() {
    file, err := os.Open("plik.txt")
    if err != nil {
        panic(err)
    }
    defer file.Close()

    tekst := "Hello, world!"
    liczba := 123

    _, err = file.WriteString(tekst + ":" + string(liczba))
    if err != nil {
        panic(err)
    }

    // Wyświetlamy zawartość pliku
    buf := make([]byte, 100)
    _, err = file.Read(buf)
    if err != nil {
        panic(err)
    }
    println(string(buf))
}
```

Output:
```Hello, world!:123```

# Głębsze spojrzenie

Język Go oferuje wiele wbudowanych funkcji do obsługi plików tekstowych. Przykładowo, używając funkcji "ReadAll" możemy odczytać całą zawartość pliku za pomocą jednego polecenia. Istnieje także możliwość dopisania tekstu do istniejącego pliku za pomocą funkcji "Append". Należy jednak pamiętać o zamknięciu pliku po zakończeniu operacji, dlatego warto korzystać z konstrukcji "defer".

# Zobacz także

- Dokumentacja języka Go: https://golang.org/doc/
- Biblioteka standardowa języka Go: https://pkg.go.dev/std
- Przykładowe projekty w języku Go: https://awesome-go.com/