---
date: 2024-01-20 17:36:20.953632-07:00
description: "How to: (Jak to zrobi\u0107:) Wykorzystaj `ToString` z formatowaniem\
  \ lub bez. Przyk\u0142ady poni\u017Cej."
lastmod: '2024-04-05T21:53:36.854393-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) Wykorzystaj `ToString` z formatowaniem lub bez."
title: "Konwersja daty na \u0142a\u0144cuch znak\xF3w"
weight: 28
---

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
