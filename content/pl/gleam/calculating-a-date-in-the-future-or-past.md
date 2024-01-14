---
title:                "Gleam: Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Istnieje wiele powodów, dla których ktoś mógłby chcieć obliczyć datę w przyszłości lub przeszłości. Może to być potrzebne w celu planowania wydarzeń, zarządzania terminami płatności lub prostego sprawdzenia, kiedy minęło dokładnie pewne zdarzenie.

## Jak To Zrobić

Aby obliczyć datę w przyszłości lub przeszłości, należy skorzystać z funkcji `Date.add` lub `Date.subtract` w języku Gleam. Za pomocą tych funkcji można określić jednostkę czasu (np. dni, miesiące, lata) oraz liczbę jednostek do dodania lub odjęcia od bieżącej daty.

```Gleam
let dzisiejsza_data = Date.now()
let data_w_przyszlosci = Date.add(dzisiejsza_data, Date.Months, 6)
let data_w_przeszlosci = Date.subtract(dzisiejsza_data, Date.Years, 3)

// Dziś jest 11 września 2021 roku
// data_w_przyszlosci będzie równa 11 marca 2022 roku
// data_w_przeszlosci będzie równa 11 września 2018 roku
```

## Głębsze Wglądy

Funkcje `Date.add` i `Date.subtract` przyjmują również dodatkowy argument - obiekt z ustawieniami czasu. Pozwala to na bardziej zaawansowane operacje, takie jak określanie daty z uwzględnieniem strefy czasowej lub ustawianie konkretnych godzin, minut i sekund.

Inną przydatną funkcją przy obliczaniu dat jest `Date.compare`, która pozwala porównać dwie daty ze sobą. Jest to szczególnie przydatne w przypadku sortowania lub sprawdzania, czy dana data jest przed lub po innej.

## Zobacz Również

- Oficjalna dokumentacja Gleam: https://gleam.run/
- Dokumentacja funkcji `Date` w języku Gleam: https://gleam.run/documentation/standard-libraries/date.html