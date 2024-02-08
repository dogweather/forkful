---
title:                "Konwersja daty na łańcuch znaków"
aliases:
- pl/c-sharp/converting-a-date-into-a-string.md
date:                  2024-01-20T17:36:20.953632-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konwersja daty na łańcuch znaków"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Konwersja daty na ciąg znaków to sposób na zmianę formatu `DateTime` na tekst. Programiści robią to, by łatwo wyświetlać i przechowywać informacje o dacie w czytelnej formie.

## How to: (Jak to zrobić:)
Wykorzystaj `ToString` z formatowaniem lub bez. Przykłady poniżej:

```C#
DateTime now = DateTime.Now;

// Bez specyficznego formatowania - domyślne
string defaultFormat = now.ToString();
Console.WriteLine(defaultFormat); // "2023-04-05 14:30:52"

// Z określonym formatowaniem - np. tylko data
string customFormat = now.ToString("yyyy-MM-dd");
Console.WriteLine(customFormat); // "2023-04-05"

// Użycie standardów kulturowych
string plFormat = now.ToString("d", new CultureInfo("pl-PL"));
Console.WriteLine(plFormat); // "05.04.2023"
```

## Deep Dive (Dogłębna analiza):
Konwersja dat na teksty jest obecna od początków programowania. W C# odbywa się głównie za pomocą metody `ToString()`. Metoda ta przyjmuje argumenty określające format wyjściowy - zarówno predefiniowane łańcuchy formatujące, jak `"G"`, `"D"`, `"yyyy-MM-dd"` czy też niestandardowe wzorce. 

Istnieją także alternatywy, jak `String.Format()` czy interpolacja stringów w nowszych wersjach C#. Ważne są też ustawienia regionalne (`CultureInfo`), które wpływają na formatowanie daty (dla Polski będzie to pl-PL).

Implementacja konwersji zależy od dwóch rzeczy: jak dane są prezentowane użytkownikowi i w jakim formacie muszą być zapisane (np. w bazie danych czy plikach). Znajomość formatów i kultur pozwala na tworzenie aplikacji wielojęzycznych i dobrze lokalizowanych.

## See Also (Zobacz również):
- [Dokumentacja metody ToString()](https://docs.microsoft.com/pl-pl/dotnet/api/system.datetime.tostring?view=net-6.0)
- [Klasy CultureInfo](https://docs.microsoft.com/pl-pl/dotnet/api/system.globalization.cultureinfo?view=net-6.0)
- [Standardowe i niestandardowe formaty daty i czasu](https://docs.microsoft.com/pl-pl/dotnet/standard/base-types/standard-date-and-time-format-strings)
- [String.Format w C#](https://docs.microsoft.com/pl-pl/dotnet/api/system.string.format?view=net-6.0)
- [Interpolacja stringów](https://docs.microsoft.com/pl-pl/dotnet/csharp/language-reference/tokens/interpolated)
