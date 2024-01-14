---
title:    "Gleam: Obliczanie daty w przyszłości lub przeszłości"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Dlaczego

Czasami, w programowaniu, musimy obliczyć datę w przyszłości lub w przeszłości. Może to być potrzebne, na przykład przy tworzeniu systemu rezerwacji lub kalendarza. Używając Gleama, jest to łatwe zadanie, które może być wykonane szybko i bezproblemowo.

## Jak to zrobić

Do obliczania daty w przyszłości lub w przeszłości w Gleam, musimy użyć modułu ```Date``` z paczki standardowej. Poniżej przedstawione są przykłady kodu dla różnych scenariuszy:

### Obliczanie daty w przyszłości

```
import Date

let now = Date.now() // Zwraca aktualną datę i czas
let future = Date.add_days(now, 7) // Dodaje 7 dni do aktualnej daty
```

W tym przykładzie, wykorzystujemy funkcję ```add_days``` aby dodać 7 dni do aktualnej daty. Istnieje wiele innych funkcji w module ```Date```, które pozwalają na manipulację datami, takie jak ```add_months```, ```add_hours``` czy ```add_years```.

### Obliczanie daty w przeszłości

```
import Date

let now = Date.now()
let past = Date.sub_days(now, 14) // Odejmuje 14 dni od aktualnej daty
```

Analogicznie, za pomocą funkcji ```sub_days``` możemy odjąć określoną liczbę dni od aktualnej daty.

## Głębszy przegląd

Obliczanie daty w przyszłości lub w przeszłości jest możliwe dzięki wykorzystaniu liczb całkowitych dla dat. W Gleam, można używać jednostki czasu, takie jak dni, miesiące czy lata, aby łatwo manipulować datami. Jest to przydatne rozwiązanie w przypadku, gdy potrzebujemy wykonać bardziej skomplikowane obliczenia dat.

## Zobacz także

- [Dokumentacja pakietu Date w Gleam](https://gleam.run/core/Date.html)
- [Przykładowy projekt Gleam - Kalendarz](https://github.com/gleam-lang/examples/tree/master/calendar)
- [Narzędzia do obliczania daty online](https://www.convertlive.com/u/convert/days/from-timestamp#Starting_timestamp_NBSP_)