---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:39.477283-07:00
description: "Wyra\u017Cenia regularne (regex) w C# s\u0105 pot\u0119\u017Cnym narz\u0119\
  dziem do dopasowywania wzorc\xF3w w ci\u0105gach znak\xF3w, co pozwala programistom\
  \ na efektywne wyszukiwanie,\u2026"
lastmod: '2024-02-25T18:49:33.762978-07:00'
model: gpt-4-0125-preview
summary: "Wyra\u017Cenia regularne (regex) w C# s\u0105 pot\u0119\u017Cnym narz\u0119\
  dziem do dopasowywania wzorc\xF3w w ci\u0105gach znak\xF3w, co pozwala programistom\
  \ na efektywne wyszukiwanie,\u2026"
title: "Korzystanie z wyra\u017Ce\u0144 regularnych"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wyrażenia regularne (regex) w C# są potężnym narzędziem do dopasowywania wzorców w ciągach znaków, co pozwala programistom na efektywne wyszukiwanie, zastępowanie, dzielenie lub ekstrakcję danych. Programiści wykorzystują wyrażenia regularne do zadań o różnym stopniu skomplikowania, począwszy od prostych walidacji, takich jak sprawdzanie formatu adresu e-mail, po złożone zadania przetwarzania tekstu, dzięki ich elastyczności i wydajności.

## Jak to zrobić:

### Proste dopasowywanie wzorców
Aby sprawdzić, czy ciąg zawiera określony wzorzec, można użyć metody `Regex.IsMatch` z przestrzeni nazw `System.Text.RegularExpressions`.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Hello, World!";
        string pattern = "World";
        bool containsPattern = Regex.IsMatch(sampleText, pattern);

        Console.WriteLine(containsPattern);  // Wyjście: True
    }
}
```

### Ekstrakcja danych
Wyodrębnianie danych z ciągu przy użyciu grup w wyrażeniu regularnym może być wykonane za pomocą metody `Regex.Match`.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Data: 2023-04-12";
        string pattern = @"Data: (\d{4})-(\d{2})-(\d{2})";
        Match match = Regex.Match(sampleText, pattern);

        if (match.Success)
        {
            Console.WriteLine($"Rok: {match.Groups[1].Value}");  // Wyjście: Rok: 2023
            Console.WriteLine($"Miesiąc: {match.Groups[2].Value}");  // Wyjście: Miesiąc: 04
            Console.WriteLine($"Dzień: {match.Groups[3].Value}");  // Wyjście: Dzień: 12
        }
    }
}
```

### Zastępowanie tekstu
Metoda `Regex.Replace` pozwala na zastąpienie tekstu w ciągu, który odpowiada określonemu wzorcowi.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Odwiedź Microsoft!";
        string pattern = "Microsoft";
        string replacement = "Google";

        string result = Regex.Replace(sampleText, pattern, replacement);

        Console.WriteLine(result);  // Wyjście: Odwiedź Google!
    }
}
```

### Dzielenie ciągów
Można podzielić ciąg na tablicę na podstawie wzorca wyrażenia regularnego przy użyciu metody `Regex.Split`.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "jeden,dwa,trzy,cztery,pięć";
        string pattern = ",";

        string[] result = Regex.Split(sampleText, pattern);

        foreach (string item in result)
        {
            Console.WriteLine(item);
        }
        // Wyjście: 
        // jeden
        // dwa
        // trzy
        // cztery
        // pięć
    }
}
```

### Korzystanie z bibliotek innych firm
Chociaż .NET Framework oferuje obszerną obsługę wyrażeń regularnych, istnieją również biblioteki innych firm, takie jak `PCRE.NET`, które oferują wyrażenia regularne zgodne z Perl (PCRE) w C#. Może to być przydatne, jeśli potrzebujesz funkcji lub składni z silnika wyrażeń regularnych Perla, które nie są dostępne w implementacji .NET.

Aby użyć `PCRE.NET`, najpierw zainstalujesz jej pakiet NuGet, a następnie możesz jej używać w sposób podobny do używania natywnych klas regex .NET.

```csharp
// Przykład z użyciem PCRE.NET tutaj
// Uwaga: Wyobraź sobie przykład podobny do tych powyżej, dostosowany do pokazania funkcji unikalnej dla PCRE.NET.
```

Przy integracji bibliotek innych firm dla wyrażeń regularnych zawsze konsultuj ich dokumentację w celu uzyskania szczegółowych informacji o użyciu i kompatybilności.
