---
title:                "Go: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Często podczas pisania programów w Go, jesteśmy zmuszeni do pracy z tekstami i prawdopodobnie pytaliśmy się już kiedyś, jak dokładnie obliczyć długość ciągu znaków w języku Go. W tym artykule dowiecie się, dlaczego jest to przydatna umiejętność w programowaniu i jak to zrobić przy użyciu prostej składni Go.

## Jak to zrobić

Do obliczania długości ciągu znaków w Go, możemy użyć wbudowanej funkcji `len()`, która zwraca liczbę bajtów w danym ciągu znaków. Oto przykładowy kod w języku Go:

```Go
ciag := "To jest przykladowy ciag znakow"
dlugosc := len(ciag)
fmt.Println(dlugosc) // wyświetli wartość 32
```

Jeśli chcemy zliczyć długość ciągu w znakach, a nie bajtach, możemy wykorzystać funkcję `RuneCountInString()`. Zwraca ona liczbę znaków unicode w danym ciągu. Poniżej znajduje się przykład kodu:

```Go
ciag := "Hello, świat!"
dlugosc := utf8.RuneCountInString(ciag)
fmt.Println(dlugosc) // wyświetli wartość 13
```

## Deep Dive

Aby lepiej zrozumieć, jak dokładnie działają funkcje `len()` i `RuneCountInString()`, warto wiedzieć trochę więcej o sposobie, w jaki Go traktuje obliczanie długości ciągów. W języku Go ciągi znaków są traktowane jako tablice bajtów, co oznacza, że funkcja `len()` zwraca liczbę bajtów w ciągu. Jednak ze względu na to, że Go obsługuje znaki unicode, niektóre znaki mogą zajmować więcej niż jeden bajt, co może wpłynąć na rezultat.

Funkcja `RuneCountInString()` z kolei iteruje przez znaki w ciągu i liczy liczbę rune (operacji, które dają nam znaki unicode) w ciągu. Dzięki temu możemy uzyskać dokładną liczbę znaków w ciągu, niezależnie od tego, ile bajtów zajmują poszczególne znaki.

## Zobacz też

- [Oficjalna dokumentacja Go](https://golang.org/doc/)
- [Porównanie funkcji `len()` i `RuneCountInString()` w języku Go](https://yourbasic.org/golang/length-of-string/)
- [Unicode w języku Go](https://blog.golang.org/strings)