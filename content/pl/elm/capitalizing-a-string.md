---
title:    "Elm: Zamienianie tekstu na wielkie litery"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

W tym blogu omówimy jak zmieniać wielkość liter w ciągu znaków w języku Elm. Jest to przydatna umiejętność, która często jest wykorzystywana w programowaniu. Nauka tej techniki pozwoli Ci na jeszcze większą kontrolę nad swoimi programami.

## Jak to zrobić

Do zmiany wielkości liter w ciągu znaków w Elm możemy użyć funkcji `String.toUpper`. Przyjmujemy w niej jako argument nasz ciąg znaków a następnie zwraca ona ten sam ciąg, ale z zamienioną na wielkie litery. Poniżej przedstawiamy przykład użycia funkcji w kontekście programowania Elm.

```Elm
string = "cześć"

uppercaseString = String.toUpper(string)

-- W wyniku otrzymujemy "CZEŚĆ"
```

## Głębszy wgląd

W języku Elm nie ma wbudowanej funkcji do zmieniania tylko wybranej części ciągu znaków na wielkie litery. Jednak istnieją biblioteki takie jak `String.Extra` lub `String.Replace` które przydadzą się w takim przypadku. Możesz także użyć funkcji `String.map` aby samodzielnie zaimplementować taką funkcjonalność. 

## Zobacz także

- [Dokumentacja Elm](https://guide.elm-lang.org/effects/string.html)
- [Kurs Elm w języku Polskim](https://elmprogramming.com/pl/)