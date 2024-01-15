---
title:                "Zamiana daty na ciąg znaków"
html_title:           "Gleam: Zamiana daty na ciąg znaków"
simple_title:         "Zamiana daty na ciąg znaków"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Czemu w ogóle miałbyś chcieć przekonwertować datę na ciąg znaków? Oto kilka powodów!

- Często w aplikacjach internetowych czy mobilnych używamy dat, a niekiedy konieczne jest prezentowanie ich w formie tekstowej.
- Przekonwertowanie daty na string pozwala na dopasowanie jej do lokalnych ustawień, takich jak format wyświetlania daty czy język.

## Jak to zrobić

Oto przykładowy kod w języku Gleam, który pokazuje, jak przekonwertować datę na string:

```Gleam
// Importowanie modułu `Date` z biblioteki standardowej
import gleam/datetime/Date

// Tworzenie daty za pomocą funkcji `new`
let my_date = Date.new(2020, 12, 31)

// Przekonwertowanie daty na string w formacie `YYYY-MM-DD`
let my_string = Date.to_string(my_date, "%Y-%m-%d")

// Wyświetlenie wyniku na konsoli
println(my_string)
```

Ten prosty kod wyświetli "2020-12-31" na konsoli.

## Pogląd w głąb

W języku Gleam istnieje kilka różnych funkcji, które pozwalają na przekonwertowanie daty na string w różnych formatach. W powyższym przykładzie użyliśmy funkcji `to_string`, ale warto również zapoznać się z funkcją `to_rfc3339`, która zwraca string w formacie RFC3339, czyli standardowym formacie używanym w komunikacji między systemami.

Ponadto, przy przekonwertowaniu daty na string, można zastosować maskę (`mask`), która pozwala na ustawienie własnego formatowania daty, np. poprzez dodanie nazw miesięcy czy dni tygodnia w języku lokalnym.

## Zobacz też

Jeśli chcesz dowiedzieć się więcej o pracy z datami w języku Gleam, polecamy zapoznanie się z oficjalną dokumentacją:

- [Oficjalna dokumentacja Date w języku Gleam](https://gleam.run/documentation#date)
- [Oficjalna dokumentacja mask w języku Gleam](https://gleam.run/documentation#mask)
- [Oficjalna dokumentacja modułu `Datetime` w języku Gleam](https://gleam.run/documentation#datetime)