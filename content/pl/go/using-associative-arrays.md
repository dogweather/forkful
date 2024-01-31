---
title:                "Korzystanie z tablic asocjacyjnych"
date:                  2024-01-30T19:11:22.861192-07:00
model:                 gpt-4-0125-preview
simple_title:         "Korzystanie z tablic asocjacyjnych"

category:             "Go"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Tablice asocjacyjne, znane w Go jako mapy, pozwalają przechowywać i uzyskiwać dostęp do danych za pomocą par klucz-wartość. Są niezbędne do zarządzania kolekcjami, gdzie można szybko wyszukać wartości za pomocą unikalnego klucza, co upraszcza manipulację danymi i ich pobieranie w programach.

## Jak to zrobić:

W Go, mapy są proste w użyciu. Oto prosty przewodnik, żeby zacząć:

1. **Deklarowanie i inicjowanie map**

```Go
package main

import "fmt"

func main() {
    // Inicjalizacja pustej mapy ze stringami jako kluczami i wartościami typu int
    var scores map[string]int
    fmt.Println(scores) // Wyświetla: map[]

    // Deklaracja i inicjalizacja niepustej mapy
    colors := map[string]string{
        "red": "#ff0000",
        "green": "#00ff00",
    }
    fmt.Println(colors) // Wyświetla: map[green:#00ff00 red:#ff0000]
}
```

2. **Dodawanie i uzyskiwanie dostępu do elementów**

```Go
func main() {
    fruits := make(map[string]int)
    fruits["apples"] = 5
    fruits["bananas"] = 10

    fmt.Println(fruits["apples"]) // Wyświetla: 5
}
```

3. **Iteracja przez mapy**

```Go
func main() {
    pets := map[string]string{"dog": "bark", "cat": "meow"}

    for key, value := range pets {
        fmt.Printf("%s goes %s\n", key, value)
    }
    // Kolejność wyjścia może się różnić, ponieważ mapy nie gwarantują kolejności.
}
```

4. **Usuwanie elementów**

```Go
func main() {
    meals := map[string]int{"breakfast": 300, "lunch": 600}
    fmt.Println(meals) // Przed usunięciem

    delete(meals, "lunch")
    fmt.Println(meals) // Po usunięciu
}
```

## Głębsze zagłębienie

Wprowadzone w Go 1, mapy zapewniają wbudowany sposób na efektywne radzenie sobie z tablicami asocjacyjnymi. W przeciwieństwie do tablic, które są uporządkowanymi kolekcjami, mapy są nieuporządkowane. Oznacza to, że kolejność iteracji po elementach mapy nie jest gwarantowana, aby być taka sama przy każdym wykonaniu, co jest kompromisem za ich zdolność do dynamicznego i znacząco elastycznego obsługiwania par klucz-wartość.

Pod maską, Go implementuje mapy jako tablice haszujące, zapewniając, że średnia złożoność dostępu, wstawiania i usuwania operacji jest O(1), w większości przypadków. Warto jednak zauważyć, że ta efektywność może się różnić w zależności od takich czynników jak kolizje haszowe.

W przypadkach użycia wymagających uporządkowanej kolejności kluczy, można rozważyć połączenie map z tablicami lub badanie pakietów stron trzecich, które oferują dodatkowe struktury danych, takie jak uporządkowane mapy lub drzewa. Pomimo ich ograniczeń, mapy Go są potężnym i niezbędnym narzędziem w wielu scenariuszach programowania.
