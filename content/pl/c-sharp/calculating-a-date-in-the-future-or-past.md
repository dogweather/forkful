---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "C#: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Obliczanie daty w przyszłości lub przeszłości to jedna z ważnych czynności, które programiści wykonują w swojej pracy. Pozwala to na szybkie wyliczenie daty na podstawie odpowiednich parametrów, co przyspiesza proces tworzenia aplikacji. W ten sposób można wyświetlić daty, zmieniać ustawienia kalendarza lub wyliczyć przyszłe wydarzenia.

## Jak to zrobić:

Można obliczyć datę w przyszłości lub przeszłości przy użyciu klasy `DateTime` w języku C#. W poniższym przykładzie wyliczymy datę 30 dni od dzisiaj:

```C#
DateTime dzis = DateTime.Now;
DateTime przyszlosc = dzis.AddDays(30);
Console.WriteLine(przyszlosc);
```
**Wynik:** 

2021-09-06 10:30:00

Aby zmienić date w przeszłości, należy użyć metody `AddDays()` z wartością ujemną, np. `-30`, co pozowli na wyliczenie daty 30 dni wstecz.

## Głębszy wgląd:
Obliczanie daty w przyszłości lub przeszłości jest możliwe dzięki liczbom całkowitym, które są przypisane do konkretnych dat. W ten sposób można obliczać daty na podstawie ustalonych algorytmów. Alternatywnym sposobem jest użycie biblioteki `DateTimeOffset`, która pozwala na obliczenia z uwzględnieniem strefy czasowej.

Warto również wspomnieć o metodzie `Add()` umożliwiającej dodanie różnych okresów czasu, takich jak tygodnie, lata czy minuty, do wybranej daty. W ten sposób można wyliczyć daty w bardziej precyzyjny sposób.

## Zobacz także:
- [Dokumentacja klasy DateTime w języku C#](https://docs.microsoft.com/pl-pl/dotnet/api/system.datetime?view=net-5.0)
- [Poradnik dotyczący operacji na dacie w C#](https://www.codecademy.com/articles/date-time-operations-csharp)
- [Przykłady użycia biblioteki DateTimeOffset](https://docs.microsoft.com/pl-pl/dotnet/standard/datetime/examples-of-using-datetimeoffset)