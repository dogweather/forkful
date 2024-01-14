---
title:    "Go: Generowanie losowych liczb"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest niezwykle ważnym aspektem programowania, który może być wykorzystywany w różnych aplikacjach i scenariuszach. Może to być przydatne do symulacji, generowania unikatowych identyfikatorów, lub sprawdzania algorytmów. Czytaj dalej, aby dowiedzieć się jak w łatwy sposób wygenerować losowe liczby w języku Go!

## Jak

```Go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

func main() {
    // Podstawowa funkcja do generowania losowych liczb
    fmt.Println(rand.Intn(100)) // wygeneruj liczbę od 0 do 99
    fmt.Println(rand.Float64()) // wygeneruj liczbę zmiennoprzecinkową od 0.0 do 1.0

    // Aby osiągnąć różne liczby za każdym razem, należy ustawić ziarno (seed)
    // Dzięki temu funkcja rand będzie korzystać z różnych generatorów
    // Możesz wybrać dowolną liczbę (np. bieżący czas)
    rand.Seed(time.Now().UnixNano())
    fmt.Println(rand.Intn(100))

    // Aby otrzymać liczbę z zakresu 10-50, można wykorzystać następującą formułę:
    fmt.Println(rand.Intn(50-10) + 10) // generuje liczbę od 10 do 49

    // Ponieważ funkcja rand bazuje na ziarnie, wynik będzie zawsze taki sam
    // aby uzyskać losowe liczby przy każdym uruchomieniu, należy ustawić zmienne ziarna
    rand.Seed(time.Now().UnixNano())
}

```

## Deep Dive

Aby głębiej zrozumieć jak działa funkcja rand w języku Go, warto wiedzieć, że bazuje ona na generatorze liczb pseudolosowych. Oznacza to, że wyniki nie są całkowicie losowe, a zależą od ustawionego ziarna. Innymi słowy, jeśli ustawisz takie same ziarno, otrzymasz taką same sekwencję liczb.

Generator pseudolosowy używany w języku Go to Xorshift, opracowany przez George Marsagliata (więcej informacji można znaleźć na stronie https://golang.org/pkg/math/rand/). Kontrolowanie ziarna pozwala na wybór określonej sekwencji liczb i daje większą kontrolę nad wynikami. Jednak należy pamiętać, że dla większości zastosowań generator pseudolosowy jest wystarczający.

## Zobacz również

- Dokumentacja języka Go dotycząca funkcji rand: https://golang.org/pkg/math/rand/
- Wideo tutorial na temat generowania liczb losowych w języku Go: https://www.youtube.com/watch?v=2s6WMIn6fjM
- Przykłady użycia generowania liczb losowych w języku Go: https://gobyexample.com/random-numbers