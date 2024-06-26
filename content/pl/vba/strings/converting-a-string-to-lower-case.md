---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:51:10.472439-07:00
description: "Jak to zrobi\u0107: W Visual Basic for Applications (VBA), konwersja\
  \ ci\u0105gu znak\xF3w na ma\u0142e litery jest prosta dzi\u0119ki funkcji `LCase`.\
  \ Funkcja ta przyjmuje ci\u0105g\u2026"
lastmod: '2024-03-13T22:44:35.216696-06:00'
model: gpt-4-0125-preview
summary: "W Visual Basic for Applications (VBA), konwersja ci\u0105gu znak\xF3w na\
  \ ma\u0142e litery jest prosta dzi\u0119ki funkcji `LCase`."
title: "Konwersja \u0142a\u0144cucha znak\xF3w na ma\u0142e litery"
weight: 4
---

## Jak to zrobić:
W Visual Basic for Applications (VBA), konwersja ciągu znaków na małe litery jest prosta dzięki funkcji `LCase`. Funkcja ta przyjmuje ciąg znaków jako wejście i zwraca nowy ciąg z wszystkimi wielkimi literami przekształconymi na małe. Oto podstawowy przykład ilustrujący to:

```basic
Dim originalString As String
Dim lowerCaseString As String

originalString = "Hello, World!"
lowerCaseString = LCase(originalString)

Debug.Print lowerCaseString ' Wynik: hello, world!
```

Możesz również użyć `LCase` bezpośrednio w porównaniach lub przypisaniach, aby uprościć kod:

```basic
If LCase(userInput) = "yes" Then
    Debug.Print "Użytkownik odpowiedział tak"
End If
```

Ten drugi przykład pokazuje, jak obsługiwać dane wejściowe użytkownika w sposób niezależny od wielkości liter, przekształcając wprowadzone dane na małe litery przed porównaniem.

## Szczegółowo
Funkcja `LCase` leży u podstaw manipulacji ciągami znaków w VBA i jest podstawową funkcją od początku istnienia języka. Ułatwia zadania związane z konwersją liter, które są powszechne w analizie danych i przetwarzaniu danych wejściowych użytkownika. Chociaż `LCase` skutecznie zaspokaja potrzebę konwersji znaków na małe litery w różnych aplikacjach, ważne jest również rozpoznanie jej ograniczeń i alternatyw.

Na przykład, chociaż `LCase` działa płynnie dla alfabetu angielskiego, obsługa języków o bardziej skomplikowanych zasadach wielkości liter może wymagać dodatkowych rozważeń lub użycia funkcji `StrConv` z odpowiednimi ustawieniami lokalizacji dla konwersji liter.

Ponadto, gdy przechodzimy z języków takich jak Python, gdzie używane jest `str.lower()`, lub JavaScript, z jego `string.toLowerCase()`, programiści mogą uznać `LCase` za prostą, ale powinni pamiętać o specyfice VBA, takiej jak brak łączenia metod.

Podsumowując, chociaż istnieją nowsze i potencjalnie bardziej potężne alternatywy w innych językach, `LCase` pozostaje niezawodną i łatwą w użyciu funkcją do konwertowania ciągów na małe litery w VBA, dobrze wpisującą się w ogólną składnię i schemat funkcjonalności języka.
