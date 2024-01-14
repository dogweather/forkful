---
title:    "Gleam: Porównywanie dwóch dat"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Dlaczego

Jedną z najczęściej wykonywanych operacji w programowaniu jest porównywanie dwóch dat. Może to być konieczne w różnych sytuacjach, na przykład podczas sprawdzania terminów ważności lub ustalania kolejności wydarzeń. W tym artykule dowiecie się, jak porównywać daty w języku programowania Gleam.

## Jak to zrobić

Porównywanie dat w Gleam jest bardzo proste i wymaga użycia tylko kilku funkcji. Poniżej przedstawiamy przykładowy kod wraz z wyjściem:

```
Gleam module ComparisonDates

fn before(date1, date2) {
  // Sprawdzenie czy data1 jest przed datą2
  date1 < date2
}

fn after(date1, date2) {
  // Sprawdzenie czy data1 jest po dacie2
  date1 > date2
}

fn equals(date1, date2) {
  // Sprawdzenie czy data1 jest równa dacie2
  date1 == date2
}

// Przykładowe dane do porównania
let date1 = Date.new(2020, 10, 1)
let date2 = Date.new(2020, 11, 1)

// Wywołanie funkcji i drukowanie wyników
IO.println("Czy data1 jest wcześniej od daty2? " ++ Bool.to_string(before(date1, date2)))
IO.println("Czy data1 jest później od daty2? " ++ Bool.to_string(after(date1, date2)))
IO.println("Czy data1 jest równa dacie2? " ++ Bool.to_string(equals(date1, date2)))

// Wyjście:
// Czy data1 jest wcześniej od daty2? true
// Czy data1 jest później od daty2? false
// Czy data1 jest równa dacie2? false
```

Jak widać, wystarczy użyć symboli `<`, `>` i `==` wraz z funkcjami `before`, `after` i `equals`, aby porównać daty w Gleam.

## Deep Dive

Porównywanie dat w Gleam różni się od innych języków programowania, ponieważ Gleam posiada wbudowany typ `Date`, który reprezentuje datę w formacie `year-month-day`. Dzięki temu nie ma potrzeby korzystania z dodatkowych bibliotek lub konwersji do innych formatów dat.

Kiedy porównujemy dwie daty, Gleam w rzeczywistości porównuje wartości liczbowe reprezentujące daty. Jeśli pierwsza data jest wcześniejsza od drugiej, to jest ona mniejsza, a jeśli jest późniejsza, to jest większa.

## Zobacz także

- [Oficjalna dokumentacja Gleam na temat typu Date](https://gleam.run/documentation/stdlib/date.html)
- [Wprowadzenie do języka Gleam](https://devstyle.pl/2021/01/20/wstep-do-gleam/)
- [Porównywanie dat w innych językach programowania](https://www.digitalocean.com/community/tutorials/how-to-compare-dates-in-javascript-php-and-python)