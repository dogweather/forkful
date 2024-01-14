---
title:    "Elm: Znajdowanie długości ciągu znaków"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego
Programowanie może wydawać się trudne, ale dzięki językowi programowania Elm może być łatwiejsze niż się wydaje. Dlatego mieszkańcy Polski powinni zacząć poznawać ten język programowania, a w dzisiejszym wpisie opowiem o tym, jak znaleźć długość ciągu znaków w Elm.

## Jak to zrobić
Aby znaleźć długość ciągu znaków w Elm, musimy użyć funkcji `String.length`, która zwraca liczbę znaków w danym ciągu. Przykładowo, jeśli mamy ciąg znaków "Elm", to `String.length "Elm"` zwróci wartość 3.

```Elm
String.length "Elm" -- 3
```

Możemy również użyć tej funkcji w połączeniu z innymi funkcjami dla bardziej skomplikowanych zadań. Na przykład, jeśli chcemy sprawdzić, czy długość ciągu znaków przekracza 10, możemy użyć funkcji `String.length` razem z funkcją `>` (większe od). Poniżej znajduje się przykładowy kod i jego wynik:

```Elm
String.length "Elm language" > 10 -- True
String.length "Programming" > 10 -- False
```

## Głębsze zanurzenie
Aby lepiej zrozumieć, jak działa funkcja `String.length` w Elm, warto przyjrzeć się jej definicji w dokumentacji języka. Dokumentacja podaje, że funkcja ta bazuje na metodzie `.length` z języka JavaScript, ale jest ona dostosowana do działania w Elm.

Zwróć uwagę, że funkcja `String.length` nie liczy jedynie liter w ciągu, ale również liczby znaków UTF-8 zawartych w tym ciągu. To oznacza, że w ciągu znaków mogą występować zarówno litery, jak i znaki specjalne i emoji, a funkcja `String.length` poprawnie określi długość takiego ciągu.

## Zobacz również
- [Dokumentacja Elm - String](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Dlaczego warto uczyć się Elm](https://elm-lang.org/)
- [Elm w praktyce - przykłady zastosowań](https://medium.com/@selleo/5x-why-elm-should-power-your-next-big-thing-fba63b3c7434)