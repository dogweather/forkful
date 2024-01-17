---
title:                "Wyszukiwanie i zamienianie tekstu."
html_title:           "C#: Wyszukiwanie i zamienianie tekstu."
simple_title:         "Wyszukiwanie i zamienianie tekstu."
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Szukanie i zamienianie tekstu to nic innego, jak zmiana wybranego tekstu na inny. Programiści często korzystają z tej metody, aby szybko i łatwo zmienić powtarzające się fragmenty w swoich programach.

## Jak to zrobić:
```C#
// Przykład kodu 
// Zmienna z tekstem, w którym chcemy zamienić wybrany fragment
string tekst = "To jest przykładowy tekst do zmiany.";

// Użycie metody Replace - pierwszy parametr to szukany tekst, drugi to tekst zastępujący
string nowyTekst = tekst.Replace("przykładowy", "bardzo fajny");

// Wynik: "To jest bardzo fajny tekst do zmiany."
Console.WriteLine(nowyTekst);
```

## Głębsze zagadnienia:
Metoda zamiany tekstu istnieje od dawna i jest używana w wielu językach programowania. Alternatywą dla użycia metody Replace jest użycie wyrażeń regularnych, które pozwalają na bardziej zaawansowaną manipulację tekstem. Implementacja tej metody może się różnić w zależności od języka programowania, lecz podstawowa zasada pozostaje taka sama - szukaj i zastępuj.

## Zobacz także:
- [Dokumentacja metody Replace w języku C#](https://docs.microsoft.com/pl-pl/dotnet/api/system.string.replace?view=net-5.0)
- [Poradnik z wyrażeniami regularnymi w C#](https://docs.microsoft.com/pl-pl/dotnet/standard/base-types/regular-expression-language-quick-reference)