---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:06:11.113478-07:00
description: "Jak to zrobi\u0107: Czytanie pliku tekstowego w Go mo\u017Cna zrealizowa\u0107\
  \ na kilka sposob\xF3w, ale jedn\u0105 z najprostszych metod jest u\u017Cycie pakietu\
  \ `ioutil`. Oto\u2026"
lastmod: '2024-03-13T22:44:34.872299-06:00'
model: gpt-4-0125-preview
summary: "Czytanie pliku tekstowego w Go mo\u017Cna zrealizowa\u0107 na kilka sposob\xF3\
  w, ale jedn\u0105 z najprostszych metod jest u\u017Cycie pakietu `ioutil`."
title: Czytanie pliku tekstowego
weight: 22
---

## Jak to zrobić:
Czytanie pliku tekstowego w Go można zrealizować na kilka sposobów, ale jedną z najprostszych metod jest użycie pakietu `ioutil`. Oto podstawowy przykład:

```go
package main

import (
    "fmt"
    "io/ioutil"
    "log"
)

func main() {
    content, err := ioutil.ReadFile("example.txt")
    if err != nil {
        log.Fatal(err)
    }

    fmt.Println(string(content))
}
```

Zakładając, że `example.txt` zawiera "Hello, Go!", program ten wyświetli:

```
Hello, Go!
```

Jednakże, począwszy od Go 1.16, pakiet `ioutil` został wycofany, i zaleca się używanie pakietów `os` i `io`. Oto jak można osiągnąć ten sam cel z użyciem tych pakietów:

```go
package main

import (
    "bufio"
    "fmt"
    "log"
    "os"
)

func main() {
    file, err := os.Open("example.txt")
    if err != nil {
        log.Fatal(err)
    }
    defer file.Close()

    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        fmt.Println(scanner.Text())
    }

    if err := scanner.Err(); err != nil {
        log.Fatal(err)
    }
}
```

To podejście jest nie tylko bardziej współczesne, ale także obsługuje większe pliki, ponieważ czyta plik linia po linii, zamiast wczytywać całą treść do pamięci naraz.

## Dogłębna analiza:
Obsługa operacji na plikach w Go, w tym czytanie z plików, odzwierciedla filozofię języka dotyczącą prostoty i wydajności. Początkowo pakiet `ioutil` oferował proste operacje na plikach. Jednakże, z rozwojem standardowej biblioteki Go i zmianą w kierunku bardziej jawnej obsługi błędów i zarządzania zasobami, pakiety `os` i `io` stały się preferowanymi alternatywami do pracy z plikami.

Te zmiany podkreślają zaangażowanie Go w wydajność i bezpieczeństwo, szczególnie w unikaniu problemów z pamięcią, które mogą wystąpić przy wczytywaniu dużych plików w całości. Metoda `bufio.Scanner` wprowadzona do czytania plików linia po linii podkreśla adaptacyjność języka i skupienie na współczesnych wyzwaniach obliczeniowych, takich jak przetwarzanie dużych zbiorów danych lub strumieniowanie danych.

Chociaż dostępne są zewnętrzne biblioteki do pracy z plikami w Go, możliwości standardowej biblioteki są często wystarczające i preferowane ze względu na ich stabilność i wydajność. Zapewnia to, że programiści Go mogą efektywnie zarządzać operacjami na plikach bez polegania na dodatkowych zależnościach, co jest zgodne z ogólnym minimalistycznym etosem i projektem języka do budowania wydajnego, niezawodnego oprogramowania.
