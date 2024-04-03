---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:14.664260-07:00
description: "Jak to zrobi\u0107: C# oferuje prost\u0105 metod\u0119 na kapitalizacj\u0119\
  \ ci\u0105g\xF3w za pomoc\u0105 wbudowanych metod. Najprostszym sposobem jest bezpo\u015B\
  rednia modyfikacja ci\u0105gu t\u0105\u2026"
lastmod: '2024-03-13T22:44:35.390603-06:00'
model: gpt-4-0125-preview
summary: "C# oferuje prost\u0105 metod\u0119 na kapitalizacj\u0119 ci\u0105g\xF3w\
  \ za pomoc\u0105 wbudowanych metod."
title: "Zamiana liter na wielkie w \u0142a\u0144cuchu znak\xF3w"
weight: 2
---

## Jak to zrobić:
C# oferuje prostą metodę na kapitalizację ciągów za pomocą wbudowanych metod. Najprostszym sposobem jest bezpośrednia modyfikacja ciągu tą metodą. Dla bardziej skomplikowanych lub specyficznych zasad kapitalizacji (np. wielka litera na początku każdego słowa) mogą być potrzebne dodatkowe biblioteki lub metody ręczne. Poniżej przedstawiono przykłady, demonstrujące, jak kapitalizować ciąg na różne sposoby w C#.

### Podstawowa kapitalizacja:
Aby uczynić wielką literą pierwszą literę pojedynczego słowa lub zdania:

```csharp
string originalString = "hello world";
string capitalizedString = char.ToUpper(originalString[0]) + originalString.Substring(1);
Console.WriteLine(capitalizedString); // Wynik: "Hello world"
```

### Kapitalizacja każdego słowa:
Aby uczynić wielką literą pierwszą literę każdego słowa w ciągu, możesz użyć metody `TextInfo.ToTitleCase` znajdującej się w przestrzeni nazw `System.Globalization`:

```csharp
using System;
using System.Globalization;

string originalString = "hello world";
TextInfo textInfo = CultureInfo.CurrentCulture.TextInfo;
string capitalizedString = textInfo.ToTitleCase(originalString);
Console.WriteLine(capitalizedString); // Wynik: "Hello World"
```

Uwaga: `ToTitleCase` nie zmienia na małe litery pozostałych liter; zmienia tylko na wielką literę pierwszą literę każdego słowa. Ponadto, w zależności od ustawień kulturowych, pewne słowa w zasadach wielkich liter (takie jak "and", "or", "of") mogą nie być kapitalizowane.

### Użycie metod rozszerzenia dla większej możliwości ponownego użycia:
Możesz stworzyć metodę rozszerzenia dla klasy `string`, aby uprościć proces kapitalizacji, czyniąc kod czystszym i bardziej reużywalnym. Oto jak stworzyć i używać takiej metody:

```csharp
using System;

public static class StringExtensions
{
    public static string Capitalize(this string input)
    {
        if (string.IsNullOrEmpty(input))
        {
            return input;
        }
        return char.ToUpper(input[0]) + input.Substring(1);
    }
}

class Program
{
    static void Main(string[] args)
    {
        string originalString = "hello world";
        string capitalizedString = originalString.Capitalize();
        Console.WriteLine(capitalizedString); // Wynik: "Hello world"
    }
}
```

Ta metoda rozszerzenia `Capitalize` może być wywołana na dowolnym obiekcie ciągu w obrębie przestrzeni nazw, oferując bardziej intuicyjne i zorientowane obiektowo podejście do manipulacji ciągami w C#.

### Biblioteki stron trzecich:
Chociaż biblioteka standardowa C# spełnia większość potrzeb dotyczących kapitalizacji ciągów, pewne wyspecjalizowane zadania mogą skorzystać z bibliotek stron trzecich, takich jak Humanizer. Jednakże, do zadań dotyczących prostego kapitalizowania ciągów lub każdego słowa w ciągu, standardowe metody C# są adekwatne i efektywne, eliminując potrzebę zewnętrznych zależności.
