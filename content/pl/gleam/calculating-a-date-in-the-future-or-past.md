---
title:                "Gleam: Obliczanie daty w przyszłości lub przeszłości."
simple_title:         "Obliczanie daty w przyszłości lub przeszłości."
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami potrzebujemy obliczyć datę w przyszłości lub w przeszłości, na przykład w celu ustalenia daty ważności produktu lub wydarzenia, lub także dla celów planowania. W tym wpisie pokażemy, jak możesz to zrobić za pomocą języka programowania Gleam.

## Jak to zrobić

Aby obliczyć datę w przyszłości lub w przeszłości w Gleam, wykorzystujemy funkcję ```Date.add/2```, która przyjmuje dwa argumenty: datę, do której chcemy dodać lub odjąć okres czasu, oraz okres czasu, który chcemy dodać lub odjąć. Na przykład, jeśli chcemy obliczyć datę 30 dni w przód od dzisiaj, użyjemy kodu:

```
import Gleam.Date

let dzisiejsza_data = Date.now()
let przyszla_data = Date.add(dzisiejsza_data, {days: 30})

IO.inspect(przyszla_data)  // {year: 2021, month: 10, day: 6}
```

Zauważ, że okres czasu musi być przekazany jako mapka z kluczami ```days```, ```months``` lub ```years```, a wartości odpowiadające tym kluczom są liczbami całkowitymi. Możemy także dodać lub odjąć więcej niż jeden rodzaj okresu, np. 2 lata i 6 miesięcy:

```
let przyszla_data = Date.add(dzisiejsza_data, {years: 2, months: 6})

IO.inspect(przyszla_data)  // {year: 2023, month: 4, day: 1}
```

## Deep Dive

W języku Gleam istnieje także funkcja ```Date.diff/2```, która pozwala obliczyć różnicę pomiędzy dwiema datami. Zwraca ona liczbę dni pomiędzy datami lub nil w przypadku, gdy jedna z dat jest wcześniejsza niż druga. Przykładowe użycie tej funkcji:

```
import Gleam.Date

let data1 = {year: 2021, month: 5, day: 20}
let data2 = {year: 2020, month: 12, day: 30}

let roznica = Date.diff(data1, data2)

IO.inspect(roznica)  // 141
```

## Zobacz także

- Dokumentacja języka Gleam: https://gleam.run/
- Wspomniana funkcja ```Date.add/2```: https://gleam.run/modules/Date.html#add
- Funkcja ```Date.diff/2```: https://gleam.run/modules/Date.html#diff