---
title:    "C#: Konwersja daty na łańcuch znaków."
keywords: ["C#"]
---

{{< edit_this_page >}}

## Dlaczego

Konwersja daty na ciąg znaków jest ważnym elementem programowania, zwłaszcza w kontekście pracy z datami i czasem. W wielu aplikacjach potrzebujemy wyświetlić datę w formie zrozumiałej dla użytkownika, a więc w postaci tekstu. W tym artykule dowiesz się, jak wykonać tę operację za pomocą języka C#.

## Jak to zrobić

Aby skonwertować datę na ciąg znaków w C#, musimy skorzystać z metody `ToString()` dostępnej w klasie `DateTime`. Poniżej przedstawione są przykładowe kody i wynik dla różnych szablonów formatów:

```C#
DateTime date = DateTime.Now;

// Data i godzina
Console.WriteLine(date.ToString("MM/dd/yyyy hh:mm tt")); // 04/17/2021 10:35 AM

// Dzień tygodnia i miesiąc
Console.WriteLine(date.ToString("ddd, MMMM d")); // Sat, April 17

// Rok, miesiąc i dzień
Console.WriteLine(date.ToString("yyyy-MM-dd")); // 2021-04-17

// Godzina w formacie 24-godzinnym
Console.WriteLine(date.ToString("HH:mm")); // 22:35
```

W powyższych przykładach wykorzystano różne szablony formatów, ale możesz także stworzyć swój własny. W celu uzyskania pełnej listy dostępnych formatów, możesz odwiedzić stronę dokumentacji Microsoft na temat [formatowania dat i godzin w C#](https://docs.microsoft.com/pl-pl/dotnet/standard/base-types/custom-date-and-time-format-strings).

## Wnikliwa analiza

Podczas konwersji daty na ciąg znaków, warto zwrócić uwagę na kilka istotnych elementów. Po pierwsze, należy określić prawidłowy strefy czasowej, ponieważ inaczej wyświetlone daty mogą różnić się w zależności od lokalizacji. Po drugie, trzeba być świadomym wybranego formatowania, ponieważ niektóre symbole (np. `MM`) mogą być mylone z innymi (np. `mm` oznacza minuty, a nie miesiące). Ważne jest także pamiętanie o obsłudze wyjątków, np. w przypadku próby konwersji pustej daty.

## Zobacz także

- [Dokumentacja Microsoft - formatowanie dat i godzin w C#](https://docs.microsoft.com/pl-pl/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [Poradnik programisty - konwertowanie daty na ciąg znaków w C#](https://www.tutorialsteacher.com/csharp/csharp-date-time)