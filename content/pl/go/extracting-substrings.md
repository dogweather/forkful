---
title:                "Go: Ekstrakcja podłańcuchów"
simple_title:         "Ekstrakcja podłańcuchów"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Ekstrakcja podciągów to podstawowa operacja w wielu językach programowania, w tym w Go. Pozwala ona na wydzielanie wybranych części ciągu znaków, co jest niezbędne w wielu sytuacjach. W tym artykule dowiecie się, jak łatwo wykonać tę operację w języku Go.

## Jak to zrobić

Aby wydobyć podciągi w Go, musimy użyć funkcji `substring`, która przyjmuje dwa argumenty: indeks początkowy i końcowy. Są one liczone od zera i mówią nam, które znaki z pierwotnego ciągu chcemy wydobyć.

```Go
package main

import "fmt"

func main() {
    str := "To jest przykładowy ciąg znaków"

    // Wydobywanie pojedynczego słowa
    fmt.Println(str[3:7]) // wyświetli "jest"

    // Wydobywanie części zdania
    fmt.Println(str[8:21]) // wyświetli "przykładowy ciąg"

    // Wydobywanie od danego indeksu do końca
    fmt.Println(str[12:]) // wyświetli "ciąg znaków"
}
```

Jak widać, za pomocą prostej funkcji możemy wydobywać dowolne części ciągu znaków. To bardzo przydatne np. podczas przetwarzania danych zebranej z użytkownika lub analizy tekstu.

## Głębsze zagadnienia

Funkcja `substring` w języku Go ma również możliwość przekazywania ujemnych indeksów, co pozwala na wydobywanie podciągów od końca ciągu. Przykładowo, `str[-5:]` będzie oznaczać wydobycie ostatnich pięciu znaków z ciągu.

Możemy także wykorzystać pętlę, aby wydobyć wiele podciągów z jednego ciągu, korzystając z różnych indeksów.

## Zobacz także

- Dokumentacja oficjalna języka Go o funkcji `substring`: https://golang.org/pkg/strings/#example_Substring
- Wideo-tutorial (po angielsku) na temat wydobywania podciągów w Go: https://www.youtube.com/watch?v=k3obgY0PuQ8
- Przykładowe zadania z wydobywaniem podciągów w języku Go: https://www.hackerrank.com/challenges/go-lang-substring