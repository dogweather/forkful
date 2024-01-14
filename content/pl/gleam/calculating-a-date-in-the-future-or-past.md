---
title:    "Gleam: Obliczanie daty w przyszłości lub przeszłości"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego?

Kalkulowanie dat w przyszłości lub przeszłości może być ważne w wielu aspektach życia, od planowania wydarzeń po korzystanie z różnych aplikacji. W tym wpisie przeczytasz, jak w prosty sposób zaimplementować tę funkcję w języku programowania Gleam.

## Jak to zrobić?

Jeśli chcesz obliczyć datę w przyszłości lub przeszłości w języku Gleam, wystarczy wykorzystać funkcję `~add_days` z biblioteki `Datetime`. Poniżej znajduje się przykładowy kod, który pokazuje, jak użyć tej funkcji, aby obliczyć datę z wybraną liczbą dni w przyszłości lub przeszłości.

```
Gleam import Datetime

let days_added = Datetime.add_days(7) 
// oblicza datę o 7 dni w przód

println(days_added) 
// wypisze datę z aktualnym rokiem, miesiącem i dniem ustawionym na 7 dni w przyszłości
```

Możesz także użyć innych funkcji z biblioteki `Datetime`, takich jak `~sub_days`, `~add_months` czy `~add_years`, aby obliczać datę z różnymi jednostkami czasu. Wszystkie te funkcje działają w podobny sposób, więc wystarczy zmienić nazwę funkcji i wpisać odpowiednią liczbę jednostek w nawiasie.

## Wnikliwszy rozkład

Dokładniejsza metoda obliczania dat w przyszłości lub przeszłości polega na wykorzystaniu biblioteki `Calendar` w języku Gleam. Biblioteka ta zawiera wiele funkcji, które umożliwiają manipulowanie i przeliczanie dat w różny sposób.

Jedną z tych funkcji jest `Calendar.encode_date`, która pozwala na stworzenie obiektu daty na podstawie podanych informacji, takich jak rok, miesiąc i dzień. Następnie możemy użyć funkcji `DateTime.to_day_of_year` w celu przeliczenia daty na jednostki czasu w roku, co jest przydatne, gdy chcemy obliczyć datę np. za 30 dni.

```
Gleam import Calendar
import DateTime

let my_date = Calendar.encode_date(2020, April, 5)
let day_of_year = DateTime.to_day_of_year(my_date)

let date_in_future = Calendar.add_days(day_of_year, 30)

println(date_in_future) 
//wypisze datę o 30 dni w przód
```

Ponadto, biblioteka `Calendar` zawiera również funkcje pozwalające na obliczanie różnych informacji o dacie, takich jak numer tygodnia czy dni pomiędzy dwiema datami. Wykorzystaj swoją wyobraźnię i spróbuj stworzyć własne funkcje do obliczania dat w przyszłości lub przeszłości!

## Zobacz też

- [Dokumentacja Gleam](https://gleam.run/standard-library/datetime.html)
- [Przykładowy projekt w języku Gleam](https://github.com/gleam-lang/gleam-cli/tree/master/examples/datetime)
- [Poradnik o języku Gleam](https://gleam.run/learn/overview.html)