---
title:                "Refaktoryzacja"
aliases: - /pl/go/refactoring.md
date:                  2024-02-03T18:07:29.251877-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refaktoryzacja"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/refactoring.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Refaktoryzacja w programowaniu polega na restrukturyzacji istniejącego kodu komputerowego — zmianie faktoringu — bez zmiany jego zewnętrznego zachowania. Programiści podejmują ten proces, aby poprawić czytelność kodu, zmniejszyć złożoność i poprawić utrzymywalność, co ostatecznie sprawia, że oprogramowanie jest łatwiejsze do zrozumienia i modyfikacji.

## Jak to zrobić:

W Go refaktoryzacja może obejmować od prostych poprawek kodu po bardziej złożone zmiany. Zaczynając od podstawowego przykładu: uproszczenie początkowej funkcji Go dla lepszej czytelności i wydajności.

**Przed refaktoryzacją:**

```go
package main

import "fmt"

func CalculatePrice(quantity int, price float64) float64 {
    var total float64
    if quantity > 0 {
        total = float64(quantity) * price
    } else {
        total = 0
    }
    return total
}

func main() {
    fmt.Println(CalculatePrice(10, 5.99))  // Wyjście: 59.9
}
```

**Po refaktoryzacji:**

```go
package main

import "fmt"

func CalculatePrice(quantity int, price float64) float64 {
    if quantity > 0 {
        return float64(quantity) * price
    }
    return 0
}

func main() {
    fmt.Println(CalculatePrice(10, 5.99))  // Wyjście: 59.9
}
```

W wersji po refaktoryzacji usunięto `else`, co upraszcza przepływ funkcji, nie wpływając na jej wyjście — przykład podstawowej, lecz wpływowej techniki refaktoryzacji w Go.

Dla bardziej zaawansowanego przykładu, rozważ refaktoryzację funkcji w celu użycia interfejsów dla lepszej wielokrotności użycia i testowalności:

**Przed refaktoryzacją:**

```go
package main

import "fmt"

type Logger struct{}

func (l Logger) Log(message string) {
    fmt.Println("Log:", message)
}

func ProcessData(data string, logger Logger) {
    // Wyobraź sobie jakąś obróbkę danych tutaj
    logger.Log("Dane przetworzone")
}

func main() {
    logger := Logger{}
    ProcessData("przykładowe dane", logger)
}
```

**Po refaktoryzacji:**

```go
package main

import "fmt"

type Logger interface {
    Log(message string)
}

type ConsoleLogger struct{}

func (c ConsoleLogger) Log(message string) {
    fmt.Println("Log:", message)
}

func ProcessData(data string, logger Logger) {
    // Obróbka danych pozostaje niezmieniona
    logger.Log("Dane przetworzone")
}

func main() {
    logger := ConsoleLogger{}
    ProcessData("przykładowe dane", logger)
}
```

Refaktoryzacja do użycia interfejsu (`Logger`) zamiast konkretnego typu (`ConsoleLogger`) poprawia elastyczność funkcji i oddziela przetwarzanie danych od specyficznej implementacji logowania.

## Głębsze spojrzenie

Refaktoryzacja w Go musi balansować między prostotą (jedną z podstawowych filozofii Go) a elastycznością potrzebną w dużych projektach oprogramowania. Biorąc pod uwagę minimalistyczne podejście Go do funkcji — bez uogólnień (do niedawna) i z silnym naciskiem na czytelność — język ten naturalnie kieruje programistów ku prostszym, bardziej utrzymywalnym strukturom kodu. Jednakże to nie oznacza, że kod Go nie korzysta z refaktoryzacji; oznacza to, że refaktoryzacja musi zawsze priorytetowo traktować jasność i prostotę.

Historycznie, brak pewnych funkcji w Go (np. uogólnień przed Go 1.18) prowadził do kreatywnych, ale czasami zawiłych rozwiązań dla ponownego wykorzystania kodu i elastyczności, czyniąc refaktoryzację dla abstrakcji powszechną praktyką. Z wprowadzeniem uogólnień w Go 1.18, programiści Go przeprowadzają teraz refaktoryzację dziedzicznych kodów, aby wykorzystać tę funkcję dla lepszego bezpieczeństwa typów i ponownego wykorzystania kodu, co demonstruje ewoluujący charakter praktyk refaktoryzacyjnych w Go.

Jednakże narzędzia Go, w tym `gofmt` do formatowania kodu i `go vet` do identyfikacji podejrzanych konstrukcji, wspomagają utrzymanie czystych baz kodów, redukując potrzebę obszernej refaktoryzacji. Choć refaktoryzacja jest nieocenionym narzędziem w arsenale programisty Go, mądre korzystanie z funkcji języka Go i narzędzi od samego początku może pomóc zminimalizować potrzebę złożonej refaktoryzacji później.
