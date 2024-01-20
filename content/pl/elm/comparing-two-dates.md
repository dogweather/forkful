---
title:                "Porównywanie dwóch dat"
html_title:           "C++: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Porównywanie dwóch dat to proces sprawdzania, która data jest wcześniejsza, późniejsza lub czy są one równe. Programiści robią to, aby prawidłowo manipulować danymi związanymi z datą w swoich aplikacjach.

## Jak to zrobić:

Elm (wersja aktualna) umożliwia proste porównywanie dat. Poniżej przedstawiono przykładowy kod:

```Elm
import Time

timeComparison : Time.Posix -> Time.Posix -> String
timeComparison time1 time2 =
    if Time.millisToPosix time1 > Time.millisToPosix time2 then
        "Time1 jest późniejszy"
    else if Time.millisToPosix time1 < Time.millisToPosix time2 then
        "Time2 jest późniejszy"
    else
        "Daty są równe"
```

W tym kodzie porównujemy dwie daty (time1 i time2) - daty są przekształcane na wartości typu `Time.Posix` za pomocą `Time.millisToPosix`, a następnie porównywane.

## Głębsze spojrzenie 

Porównywanie dat jest od dawna istotnym aspektem programowania. Elm musiał dostosować się do tej potrzeby, podobnie jak wiele innych języków programowania.

Alternatywą dla porównywania dat w Elm jest użycie języka JavaScript w połączeniu z pakietem Elm/JavaScript. Możemy następnie korzystać z wbudowanych funkcji JavaScript do porównywania dat. Jest to jednak mniej eleganckie i wymaga dodatkowego kodu.

Szczegóły implementacji porównywania dat w Elm są dosyć proste: Elm przechowuje daty jako milisekundy od pewnego ustalonego punktu (epoki Unix), co umożliwia łatwe porównywanie tych wartości.

## Zobacz także

Jeżeli chcesz dowiedzieć się więcej na temat dat i czasu w Elm, zobacz poniższe linki:

- [Dokumentacja Elm na temat czasu (Time)](https://package.elm-lang.org/packages/elm/time/latest/)