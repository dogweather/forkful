---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "Elm: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

Co to jest wyliczanie dat w przyszłości i przeszłości, i dlaczego programiści to robią?

Wyliczanie dat w przyszłości i przeszłości jest procesem określania daty, która jest odległa od bieżącej daty o określoną ilość dni, tygodni, miesięcy lub lat. Jest to powszechna praktyka w programowaniu, ponieważ często potrzebujemy ustawić datę wydarzenia w przyszłości lub przeszłości, na przykład do harmonogramowania zadań lub szacowania czasu trwania projektu.

Jak to zrobić?

Elm jest językiem programowania, który oferuje wiele wbudowanych funkcji do pracy z datami. Jedną z tych funkcji jest `Date.add`, która przyjmuje trzy argumenty: jednostkę czasu (dzień, tydzień, miesiąc lub rok), liczbę jednostek i datę początkową, do której ma zostać dodana liczba jednostek. Przykładowy kod wyglądałby tak:

```
Elm Date.add Day 5 (Date.fromTime 1529918400000)
```

Ten kod dodaje 5 dni do daty 26 czerwca 2018 (reprezentowanej przez timestamp 1529918400000) i zwraca nową datę 1 lipca 2018. Możemy również dodać jednostki tygodni, miesięcy lub lat, zmieniając odpowiednio pierwszy argument.

Ciekawostka: Zanim do elm.core została dodana funkcja `Date.add`, programiści musieli korzystać z funkcji `Date.toUtc`, `Date.toLocal` i `Date.fromTimezone` do manipulacji datami. Teraz, dzięki `Date.add`, jest to znacznie łatwiejsze i bardziej intuicyjne.

Czym jest wyliczanie dat w przyszłości i przeszłości?

Historia wyliczania dat sięga starożytności, kiedy ludzie zaczęli używać kalendarza słonecznego do śledzenia czasu. Dziś istnieją różne systemy kalendarzowe, takie jak kalendarz gregoriański, kalendarz juliański i kalendarz chiński. W każdym systemie data ma inny sposób reprezentacji i obliczania.

Alternatywą dla funkcji `Date.add` w Elm jest biblioteka zewnętrzna, taka jak Richard Feldman's Elm Time, która oferuje bardziej zaawansowane funkcje do pracy z datami. Możesz również użyć języka programowania, który ma wbudowane funkcje do pracy z datami, takie jak JavaScript lub Java.

Podsumowując, wyliczanie dat w przyszłości i przeszłości jest niezbędną umiejętnością w programowaniu. Elm oferuje proste i wygodne narzędzia do manipulacji datami, które ułatwiają pracę z nimi. Nie zapomnij jednak, że istnieją też inne opcje, jeśli potrzebujesz bardziej zaawansowanej funkcjonalności.

Zobacz także:

1. Dokumentacja funkcji `Date.add` w Elm: https://package.elm-lang.org/packages/elm/core/latest/Date#add
2. Biblioteka zewnętrzna Elm Time: https://package.elm-lang.org/packages/elm/time/latest/
3. Porównanie najpopularniejszych języków programowania: https://www.hackerrank.com/developer-skill-set-summary-2018