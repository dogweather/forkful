---
title:                "Konwertowanie daty na ciąg znaków"
html_title:           "C#: Konwertowanie daty na ciąg znaków"
simple_title:         "Konwertowanie daty na ciąg znaków"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Zamiana daty na ciąg znaków to proces przekształcania danych z jednego formatu na inny. Programiści zazwyczaj wykonują ten krok, gdy potrzebują wyświetlić datę w formie zrozumiałej dla użytkownika lub jeśli muszą porównać dwie daty. Jest to również przydatne podczas przechowywania dat w bazie danych lub przesyłania ich przez sieć.

## Jak to zrobić:

Oto prosty przykład w C#, który pokazuje, jak zamienić aktualną datę na ciąg znaków w formacie "MM/dd/yyyy":

```
C# DateTime now = DateTime.Now;
Console.WriteLine(now.ToString("MM/dd/yyyy")); // Output: 07/24/2021
```

Można również dostosować formatowanie, np. dodać godzinę i minutę:

```
C# DateTime now = DateTime.Now;
Console.WriteLine(now.ToString("MM/dd/yyyy HH:mm")); // Output: 07/24/2021 13:56
```

## Głębsza analiza:

Zamiana daty na ciąg znaków jest powszechnie stosowaną techniką w programowaniu. Wczesne języki takie jak BASIC, nie miały wbudowanych funkcji do formatowania dat, więc programiści musieli pisać własne metody do tego celu. Jedną z alternatyw jest wykorzystanie biblioteki DateTime zamiast bezpośrednio parsować datę do ciągu znaków. W implementacji tej metody stosowane są różne formaty dat, np. ISO-8601.

## Zobacz również:

- [Dokumentacja C# - Metoda DateTime.ToString](https://docs.microsoft.com/pl-pl/dotnet/api/system.datetime.tostring)
- [Formaty dat w C#](https://docs.microsoft.com/pl-pl/dotnet/standard/base-types/standard-date-and-time-format-strings)
- [ISO-8601](https://pl.wikipedia.org/wiki/ISO-8601)