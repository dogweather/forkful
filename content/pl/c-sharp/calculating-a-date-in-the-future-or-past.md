---
title:                "C#: Obliczanie daty w przyszłości lub przeszłości"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego?

Obliczenie daty w przyszłości lub przeszłości może być niezbędne w wielu różnych programach. Na przykład, może być potrzebne w celu ustalenia daty ważności dokumentu lub przewidywania daty dostawy paczki. W artykule tym dowiesz się, jak możesz łatwo obliczyć datę w przyszłości lub przeszłości za pomocą języka programowania C#.

## Jak to zrobić?

Obliczanie daty w przyszłości lub przeszłości w języku C# jest bardzo proste. Wystarczy użyć klasy `DateTime` i jej metod `AddDays()` lub `AddYears()`. Poniżej znajduje się przykładowy kod, który oblicza datę 14 dni od dzisiejszego dnia:

```C#
DateTime dzisiaj = DateTime.Today;
DateTime przyszlosc = dzisiaj.AddDays(14);
Console.WriteLine(przyszlosc);
```

Wykorzystując metodę `AddYears()`, możemy obliczyć datę w przyszłości lub przeszłości w oparciu o rok:

```C#
DateTime dzisiaj = DateTime.Today;
DateTime przeszlosc = dzisiaj.AddYears(-3);
Console.WriteLine(przeszlosc);
```

Wynik:

```
27.04.2018 00:00:00
```

Mamy również możliwość dodawania lub odejmowania kilku lat lub dni jednocześnie, np.:

```C#
DateTime dzisiaj = DateTime.Today;
DateTime przyszlosc = dzisiaj.AddDays(14).AddYears(1);
Console.WriteLine(przyszlosc);
```

Wynik:

```
09.05.2020 00:00:00
```

## Deep Dive

W języku C# istnieje wiele różnych metod do obliczania daty w przyszłości lub przeszłości. Poniżej przedstawiamy kilka przydatnych funkcji:

- `AddDays()` - dodaje określoną liczbę dni do bieżącej daty.
- `AddYears()` - dodaje określoną liczbę lat do bieżącej daty.
- `AddMonths()` - dodaje określoną liczbę miesięcy do bieżącej daty.
- `AddMilliseconds()` - dodaje określoną liczbę milisekund do bieżącej daty.
- `AddTicks()` - dodaje określoną liczbę tików (jednostka czasu używana w .NET) do bieżącej daty.

Warto również pamiętać, że klasa `DateTime` posiada wiele innych przydatnych właściwości i metod, na przykład `DayOfWeek` lub `ToString()`.

## Zobacz również

- [Dokumentacja Microsoft dotycząca klasy DateTime w języku C#](https://docs.microsoft.com/pl-pl/dotnet/api/system.datetime?view=net-5.0)
- [Poradnik na temat obliczania daty w przyszłości i przeszłości w języku C#](https://www.tutorialspoint.com/csharp/csharp_date_time.htm)
- [Przykładowy kod obliczający datę w przyszłości w języku C#](https://gist.github.com/gauravdhiman/ebf2407077e78a5faf0b)

Dziękujemy za przeczytanie tego artykułu. Mam nadzieję, że teraz wiesz, jak łatwo i szybko obliczyć datę w przyszłości lub przeszłości za pomocą języka C#. Powodzenia w twoich projektach! :)