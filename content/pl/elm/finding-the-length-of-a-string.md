---
title:                "Elm: Znajdując długość ciągu znaków"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego chcielibyśmy znaleźć długość ciągu znaków? To proste - jest to podstawowa umiejętność w programowaniu, która jest niezbędna do rozwiązywania wielu różnych problemów. W tym artykule dowiemy się, jak znaleźć długość ciągu znaków w Elm i przedstawimy kilka przydatnych przykładów.

## Jak to zrobić

Aby znaleźć długość ciągu znaków w Elm, musimy użyć funkcji `String.length`. Wygląda to następująco:

```Elm
String.length "Hello World!" -- 12
```

Funkcja ta zwraca liczbę znaków w podanym ciągu, w tym przypadku jest to 12. Dzięki temu prostemu przykładowi możemy już wiedzieć, jak używać tej funkcji w naszych programach.

W przypadku, gdy chcemy znaleźć długość ciągu znaków zapisanego w zmiennej, musimy najpierw przypisać ten ciąg do zmiennej, a następnie użyć funkcji `String.length` na tej zmiennej. Przykład:

```Elm
let message = "To jest przykładowa wiadomość!"
String.length message -- 29
```

Możemy również użyć tej funkcji wewnątrz innych wyrażeń, na przykład w warunkowym wyrażeniu `if...then`:

```Elm
if String.length "Hello" > 5 then "Zbyt długi ciąg!" else "Wszystko w porządku!"
```

W tym przypadku, jeśli długość ciągu "Hello" jest większa niż 5, zostanie zwrócony napis "Zbyt długi ciąg!", w przeciwnym wypadku zostanie zwrócony napis "Wszystko w porządku!".

## Głębsza analiza

Oprócz prostego użycia funkcji `String.length`, warto również wiedzieć, że ta funkcja liczy także whitespaces (spacje, tabulacje itp.) jako znaki. Dzięki temu możemy uzyskać dokładną liczbę znaków w ciągu, nawet jeśli zawiera on puste miejsca.

Ponadto, jeśli chcemy sprawdzić, czy długość ciągu wynosi 0, możemy użyć funkcji `String.isEmpty`, która zwraca wartość boolean (true lub false) w zależności od wyniku.

## Zobacz również

Zapoznaj się również z innymi przydatnymi funkcjami do pracy z ciągami znaków w Elm:

- `String.reverse` - odwraca ciąg znaków
- `String.contains` - sprawdza, czy podany ciąg zawiera się w innym ciągu
- `String.split` - dzieli ciąg na podciągi na podstawie podanego separatora

Zachęcamy również do zapoznania się z dokumentacją języka Elm, gdzie znajduje się pełna lista funkcji do pracy z ciągami znaków: https://package.elm-lang.org/packages/elm/core/latest/String