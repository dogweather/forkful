---
title:    "Go: Konwertowanie ciągu znaków na małe litery"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego
Konwersja tekstu na małe litery jest częstym zadaniem w wielu programach i skryptach. Może być przydatna na przykład przy walidacji danych, wyświetlaniu tekstu w jednolitym formacie lub sortowaniu. W tym wpisie dowiesz się, jak w języku Go wykonać tę operację.

## Jak to zrobić
Pierwszym krokiem jest zaimportowanie pakietu "strings", który zawiera funkcję "ToLower". Następnie należy podać jako argument tekst, który chcemy przekonwertować. Funkcja zwróci nam przekonwertowany tekst.

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    text := "PRZYKŁADOWY TEKST"
    lower := strings.ToLower(text)
    fmt.Println(lower) //Output: przykładowy tekst
}
```

## Głębszy wgląd
Warto zwrócić uwagę, że funkcja "ToLower" z pakietu "strings" nie zmienia oryginalnego tekstu, tylko zwraca nowy tekst z przekonwertowanymi małymi literami. Ponadto, funkcja ta działa szczególnie dobrze na tekstach w języku angielskim, ale może nie być odpowiednia dla innych języków, w których znaki nie są przekonwertowane na odpowiednie małe litery.

## Zobacz też

- Dokumentacja funkcji "ToLower" w języku Go: https://golang.org/pkg/strings/#ToLower
- Porównanie wydajności funkcji "ToLower" z innymi metodami konwersji w języku Go: https://flaviocopes.com/golang-case-conversion/
- Przykłady zastosowań konwersji tekstu na małe litery w projekcie: https://www.calhoun.io/5-useful-ways-to-use-case-conversion-in-go/