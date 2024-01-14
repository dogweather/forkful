---
title:    "Go: Odczytywanie argumentów wiersza poleceń"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Dlaczego

Odczytywanie argumentów wiersza poleceń jest nieodłączną częścią wielu programów. Jest to nie tylko przydatne, ale także niezbędne w niektórych przypadkach. W tej krótkiej instrukcji dowiesz się, jak odczytać argumenty wiersza poleceń w języku Go i wykorzystać je w swoich projektach.

## Jak to zrobić

Pierwszym krokiem jest zaimportowanie pakietu `os` w celu uzyskania dostępu do funkcji do odczytywania argumentów wiersza poleceń. Następnie, wewnątrz funkcji `main`, użyj funkcji `os.Args`, która zwraca tablicę stringów z wszystkimi argumentami podanymi przy uruchomieniu aplikacji.

```Go
import "os"

func main() {
    args := os.Args

    // przetwarzanie argumentów tutaj
}
```

Dzięki temu, możesz odwoływać się do pojedynczych argumentów za pomocą indeksów, np. `args[0]` to nazwa programu, a `args[1]` to pierwszy argument wiersza poleceń. Możesz również użyć pętli `for` do iteracji przez całą tablicę argumentów.

```Go
for i, arg := range args {
    fmt.Printf("Argument #%d: %s\n", i, arg)
}
```

Możesz również wyświetlić aktualną liczbę argumentów używając funkcji `len` w następujący sposób:

```Go
argsCount := len(args)
fmt.Printf("Liczba argumentów: %d\n", argsCount)
```

## Deep Dive

Dla bardziej zaawansowanych potrzeb, warto wiedzieć, że argumenty wiersza poleceń są przechowywane jako slice (rodzaj zmiennej tablicowej) w programie Go, co oznacza że możesz manipulować nimi tak, jak wszelkimi innymi slice'ami. Możesz na przykład użyć funkcji `append` do dodawania nowych argumentów do listy, lub `delete` do usuwania istniejących. Możesz także korzystać z innych funkcji z pakietu `os` w celu uzyskania dodatkowych informacji o środowisku uruchamiania aplikacji.

## Zobacz też

- [Dokumentacja pakietu os w języku Go](https://golang.org/pkg/os/)
- [Wykorzystanie flag do odczytywania argumentów wiersza poleceń w języku Go](https://gobyexample.com/command-line-flags)
- [Porównanie odczytywania argumentów wiersza poleceń w różnych językach programowania](https://www.linux.com/news/different-ways-parsing-command-line-arguments-python-and-go/)