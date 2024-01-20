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

## Co i dlaczego?
Obliczanie daty w przyszłości lub przeszłości to technika pozwalająca manipulować datami w programowaniu. Programiści robią to, aby przewidzieć, przeliczyć lub śledzić różne zdarzenia względem czasu.

## Jak to zrobić:
Poniżej znajduje się proste użycie metody `AddDays` w C#:

```C#
DateTime teraz = DateTime.Now;
Console.WriteLine("Teraz: " + teraz);
DateTime jutro = teraz.AddDays(1);
Console.WriteLine("Jutro: " + jutro);
```
Tutaj obliczyliśmy datę jutrzejszą. Przykładowy wynik:

```C#
Teraz: 19-10-2022 12:35:08
Jutro: 20-10-2022 12:35:08
```
Funktacja `AddDays` umożliwia również obliczanie daty w przeszłości (używając wartości ujemnych):

```C#
DateTime wtedy = teraz.AddDays(-10);
Console.WriteLine("10 dni temu: " + wtedy);
```
Przykładowy wynik:

```C#
10 dni temu: 09-10-2022 12:35:08
```
## Deep Dive
Historia: Obliczanie daty w przyszłości i przeszłości jest starym zabiegiem w programowaniu, obecnym od początków tego rzemiosła.

Alternatywy: Alternatywą dla metody `AddDays` jest użycie metody `Subtract`:

```C#
DateTime dziesiecDni = TimeSpan.FromDays(10);
DateTime dziesiecDniWstecz = DateTime.Now.Subtract(dziesiecDni);
```
Szczegóły implementacji: W C#, zarówno `AddDays`, `AddHours`, `AddMinutes`, etc., jak i `Subtract` są członkami klasy `System.DateTime`. `Add` zmienia datę o podaną ilość, podczas gdy `Subtract` odejmuje datę od innej daty, zwracając różnicę jako `TimeSpan`.

## Zobacz także
Aby dowiedzieć się więcej o klasie DateTime, odwiedź [Microsoft Documentation](https://docs.microsoft.com/pl-pl/dotnet/api/system.datetime?view=net-6.0). Możesz też zerknąć na [TimeSpan in C#](https://docs.microsoft.com/pl-pl/dotnet/api/system.timespan?view=net-6.0)