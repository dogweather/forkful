---
title:    "Gleam: Porównywanie dwóch dat"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dwóch dat może wydawać się niewystarczająco ważnym zadaniem w programowaniu, ale w rzeczywistości jest to niezwykle przydatna umiejętność. Pozwala na sprawdzenie, czy dana data znajduje się przed, w trakcie czy po innej dacie, co może mieć duże znaczenie w różnych aplikacjach.

## Jak to zrobić

Aby porównać dwie daty w języku programowania Gleam, można użyć funkcji `Date.compare` i podać dwa argumenty - dwa obiekty dat, które chcemy porównać. Na przykład:

```
Gleam import Date

Date.compare(Date.from_string("2021-01-01"), Date.from_string("2021-12-31"))
```

Powyższy kod zwróci wartość `-1`, ponieważ pierwsza data jest wcześniejsza niż druga. Inne możliwe wartości to `0` (gdy daty są równe) oraz `1` (gdy pierwsza data jest późniejsza niż druga).

## Dogłębna analiza

Aby lepiej zrozumieć działanie funkcji `Date.compare`, warto mieć pojęcie o formacie dat używanym przez Gleam. W tym języku, daty są przechowywane jako obiekty typu `Core.Date`, który zawiera informacje o roczu, miesiącu, dniu, godzinie, minucie i sekundzie. W praktyce jednak, rzadko kiedy trzeba ręcznie tworzyć obiekt `Core.Date` - łatwiej jest skorzystać z funkcji pomocniczych takich jak `Date.from_string` lub `Date.from_tuple`.

Inną przydatną funkcją jest `Date.diff`, która pozwala na obliczenie różnicy pomiędzy dwiema datami w wybranych jednostkach czasu (np. dniach, godzinach, minutach). Jest to szczególnie przydatne w przypadku tworzenia aplikacji związanych z zarządzaniem czasem lub wyświetlaniem dat.

## Zobacz też

- [Dokumentacja funkcji Date.compare w języku Gleam](https://gleam.run/modules/core/Date.html#compare/2)
- [Inne przydatne funkcje związane z datami w języku Gleam](https://gleam.run/modules/core/Date.html)