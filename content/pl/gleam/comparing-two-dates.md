---
title:                "Porównywanie dwóch dat"
html_title:           "Gleam: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Co & dlaczego?
Porównywanie dwóch dat jest procesem, w którym programista porównuje dwie daty, aby określić, która jest wcześniejsza lub późniejsza. Jest to powszechna czynność w programowaniu, ponieważ pozwala nam na wykrycie różnic między datami i na podjęcie odpowiednich działań w zależności od wyniku porównania.

## Jak to zrobić:
Przykłady kodu i wyjścia można znaleźć w poniższych blokach kodu `Gleam ...`:

```Gleam
import Time

Time.is_before(Time.date(2021, 04, 20), Time.date(2021, 04, 18)) // Wynik: false
Time.is_same_or_before(Time.date(2020, 12, 25), Time.date(2020, 12, 31)) // Wynik: true
Time.compare(Time.date(2021, 03, 15), Time.date(2019, 05, 01)) // Wynik: greater_than
```

Funkcja `is_before` sprawdza, czy pierwsza data jest wcześniejsza niż druga. `is_same_or_before` porównuje dwie daty i zwraca `true`, jeśli są takie same lub jeśli pierwsza data jest wcześniejsza od drugiej. Natomiast funkcja `compare` zwraca jeden z trzech wyników: `greater_than` (pierwsza data jest późniejsza), `less_than`(pierwsza data jest wcześniejsza) lub `equal` (daty są takie same).

## Głębszy zanurzenie:
Porównywanie dat jest ważne w programowaniu, ponieważ pomaga nam w analizie i manipulacji danymi. Istnieje również wiele innych sposobów na porównywanie dat w innych językach programowania, takich jak `DateTime.Compare` w C# czy `before?` w Ruby. W Gleam funkcje `is_before`, `is_same_or_before` i `compare` wykorzystują operatory porównywania (`<`, `<=` i `==`) do porównywania dat. W kolejnych wersjach języka można spodziewać się dodatkowych funkcji do porównywania, takich jak porównywanie godzin lub uwzględnianie stref czasowych.

## Zobacz także:
Dowiedz się więcej o funkcjach porównywania dat w języku Gleam w [dokumentacji](https://gleam.run/documentation/general/dates-and-times/) języka. Możesz także przeczytać o porównywaniu dat w innych językach programowania, takich jak C# i Ruby, aby lepiej porównać różnice między nimi.