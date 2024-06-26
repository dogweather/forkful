---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:06:31.194111-07:00
description: "Jak to zrobi\u0107: Go zapewnia bezpo\u015Bredni dost\u0119p do argument\xF3\
  w wiersza polece\u0144 poprzez pakiet `os`, a konkretnie za pomoc\u0105 `os.Args`,\
  \ tablicy ci\u0105g\xF3w znak\xF3w.\u2026"
lastmod: '2024-03-13T22:44:34.869659-06:00'
model: gpt-4-0125-preview
summary: "Go zapewnia bezpo\u015Bredni dost\u0119p do argument\xF3w wiersza polece\u0144\
  \ poprzez pakiet `os`, a konkretnie za pomoc\u0105 `os.Args`, tablicy ci\u0105g\xF3\
  w znak\xF3w."
title: "Czytanie argument\xF3w z linii polece\u0144"
weight: 23
---

## Jak to zrobić:
Go zapewnia bezpośredni dostęp do argumentów wiersza poleceń poprzez pakiet `os`, a konkretnie za pomocą `os.Args`, tablicy ciągów znaków. Oto prosty przykład, aby zacząć:

```go
package main

import (
    "fmt"
    "os"
)

func main() {
    // os.Args zapewnia dostęp do surowych argumentów wiersza poleceń
    fmt.Println("Argumenty wiersza poleceń:", os.Args)

    if len(os.Args) > 1 {
        // Pętla przez argumenty, pomijając pierwszy (nazwę programu)
        for i, arg := range os.Args[1:] {
            fmt.Printf("Argument %d: %s\n", i+1, arg)
        }
    } else {
        fmt.Println("Nie podano argumentów wiersza poleceń.")
    }
}
```

Przykładowe wyjście przy uruchomieniu z `go run twojprogram.go arg1 arg2` może wyglądać tak:

```
Argumenty wiersza poleceń: [/tmp/go-build123456789/b001/exe/twojprogram arg1 arg2]
Argument 1: arg1
Argument 2: arg2
```

To wypisuje wszystkie argumenty, włączając w to nazwę programu (często na indeksie 0), a następnie iteruje przez każdy podany argument, wypisując je. Dla bardziej kontrolowanego parsowania argumentów, można rozważyć pakiet `flag` do parsowania opcji wiersza poleceń.

## Szczegółowa analiza
Historycznie, dostęp do argumentów wiersza poleceń to praktyka tak stara jak programowanie w C, gdzie `argc` i `argv[]` służą podobnemu celowi. W Go, `os.Args` jest prosty, ale celowo podstawowy. Dla bardziej skomplikowanych scenariuszy, takich jak obsługa flag lub opcji, Go oferuje pakiet `flag`, który zapewnia solidne możliwości parsowania. Można to uznać za "lepszą" alternatywę, gdy aplikacja wymaga czegoś więcej niż tylko argumentów pozycyjnych.

W przeciwieństwie do niektórych języków skryptowych, które oferują wbudowane parsowanie argumentów wiersza poleceń do asocjacyjnych tablic lub obiektów, podejście Go wymaga, aby programiści sami zajęli się parsowaniem manualnie za pomocą `os.Args` dla podstawowych potrzeb lub wykorzystali pakiet `flag` dla bardziej zaawansowanych scenariuszy. Ta koncepcja odzwierciedla filozofię Go, polegającą na utrzymaniu prostoty języka podstawowego, jednocześnie zapewniając potężne biblioteki standardowe do wspólnych zadań. Chociaż może to wprowadzić niewielką krzywą uczenia się dla osób przyzwyczajonych do wbudowanego parsowania, oferuje większą elastyczność i zachęca do głębszego zrozumienia obsługi argumentów wiersza poleceń.
