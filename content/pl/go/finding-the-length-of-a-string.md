---
title:                "Go: Znajdowanie długości ciągu znaków"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Napisanie kodu w Go może być fascynującym wyzwaniem dla programistów. Jedną z podstawowych umiejętności jest znajomość różnych sposobów manipulacji ze zmiennymi, w tym również ze stringami. Poznanie sposobu wyznaczania długości stringa może znacząco ułatwić pisanie kodu w Go. 

## Jak to zrobić

Aby znaleźć długość stringa w Go, użyjemy wbudowanej funkcji `len()`, która zwraca liczbę bajtów w stringu. Przykładowy kod wygląda następująco:

```Go
package main

import "fmt"

func main() {
    str := "Cześć świecie!"     // przykładowy string
    fmt.Println(len(str))
}
```

W powyższym przykładzie, funkcja `len()` zostaje wywołana na zmiennej `str`, a wynik jest wypisany za pomocą funkcji `Println()` z pakietu `fmt`. Wyświetlony zostanie wynik 14, ponieważ string `Cześć świecie!` składa się z 14 bajtów. 

Inną techniką jest użycie pętli `for` wraz z indeksowaniem stringa w celu sprawdzenia jego długości:

````Go
package main

import "fmt"

func main() {
    str := "Jestem programistą"     // przykładowy string
    count := 0     // zmienna zliczająca długość stringa
    for range str {
    	count++
    }
    fmt.Println(count)     // wyświetli 19, długość stringa
}
````

## Głębsza analiza

W przypadku gdy string zawiera znaki nie-ASCII, znaków takich jak polskie litery, musimy pamiętać, że funkcja `len()` zwraca liczbę bajtów, a nie liczby znaków. W takim przypadku, należy użyć pętli `for` i funkcji `rune` do zliczenia liczby znaków w stringu.

````Go
package main

import "fmt"

func main() {
    str := "Święta Bożego Narodzenia"     // przykładowy string
    count := 0     // zmienna zliczająca liczbę znaków
    for range str {
        count++
    }
    fmt.Println(count)     // wyświetli 26
}
````

## Zobacz również

- [Go tutorial: Strings] (https://tour.golang.org/basics/5)
- [Go language specification for strings] (https://golang.org/ref/spec#String_types)