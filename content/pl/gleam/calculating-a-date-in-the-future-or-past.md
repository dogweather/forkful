---
title:                "Obliczanie daty w przyszłości lub w przeszłości"
html_title:           "Gleam: Obliczanie daty w przyszłości lub w przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub w przeszłości"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Planowanie przyszłości jest ważne dla większości ludzi. Czasami musimy wyznaczyć datę w przyszłości lub w przeszłości dla różnych celów, takich jak rezerwowanie wakacji lub rejestracja na wydarzenia. W tym artykule dowiesz się, jak za pomocą języka programowania Gleam możesz łatwo obliczać daty w przyszłości lub w przeszłości.

## Jak to zrobić

Aby obliczyć datę w przyszłości lub w przeszłości w języku Gleam, musisz użyć biblioteki `time` dostępnej w standardowej bibliotece. Najpierw musisz zaimportować bibliotekę za pomocą `import time`.

### Obliczanie daty w przyszłości

Aby obliczyć datę w przyszłości, musisz podać datę początkową oraz ilość wybranych jednostek czasu, jakie chcesz dodać do tej daty. Na przykład, jeśli chcesz dodać 10 dni do dzisiejszej daty, możesz użyć następującego kodu:

```Gleam
let dzisiaj = Time.now()
let dzien = Time.days(10)
let przyszla_data = Time.add(dzisiaj, dzien)
```

W powyższym przykładzie używamy funkcji `Time.days()` do określenia liczby dni oraz funkcji `Time.add()` do dodania tych dni do dzisiejszej daty. Wynik zostanie zapisany w zmiennej `przyszla_data`.

### Obliczanie daty w przeszłości

Podobnie jak w przypadku obliczania daty w przyszłości, musisz podać datę początkową oraz ilość wybranych jednostek czasu, jakie chcesz odjąć od tej daty. Na przykład, jeśli chcesz odjąć 5 tygodni od dzisiejszej daty, możesz użyć następującego kodu:

```Gleam
let dzisiaj = Time.now()
let tydzien = Time.weeks(5)
let przeszla_data = Time.sub(dzisiaj, tydzien)
```

W powyższym przykładzie używamy funkcji `Time.weeks()` do określenia liczby tygodni oraz funkcji `Time.sub()` do odjęcia tych tygodni od dzisiejszej daty. Wynik zostanie zapisany w zmiennej `przeszla_data`.

## Deep Dive

Biblioteka `time` oferuje również inne przydatne funkcje, takie jak obliczanie różnicy między dwoma datami, formatowanie daty w wybranym formacie oraz przekształcanie daty do innej strefy czasowej. Więcej informacji na ten temat znajdziesz w [dokumentacji biblioteki](https://gleam.run/libraries/time).

## See Also

- [Oficjalna strona języka Gleam](https://gleam.run/)
- [Dokumentacja języka Gleam](https://gleam.run/docs/)
- [Repozytorium Github biblioteki time](https://github.com/gleam-lang/time)