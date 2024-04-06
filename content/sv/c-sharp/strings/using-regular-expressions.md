---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:36.377958-07:00
description: "Hur man g\xF6r: F\xF6r att kontrollera om en str\xE4ng inneh\xE5ller\
  \ ett specifikt m\xF6nster kan du anv\xE4nda metoden `Regex.IsMatch` fr\xE5n namnrymden\u2026"
lastmod: '2024-03-13T22:44:37.903085-06:00'
model: gpt-4-0125-preview
summary: "F\xF6r att kontrollera om en str\xE4ng inneh\xE5ller ett specifikt m\xF6\
  nster kan du anv\xE4nda metoden `Regex.IsMatch` fr\xE5n namnrymden `System.Text.RegularExpressions`."
title: "Att anv\xE4nda regulj\xE4ra uttryck"
weight: 11
---

## Hur man gör:


### Enkel Mönsterpassning
För att kontrollera om en sträng innehåller ett specifikt mönster kan du använda metoden `Regex.IsMatch` från namnrymden `System.Text.RegularExpressions`.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Hej, världen!";
        string pattern = "världen";
        bool containsPattern = Regex.IsMatch(sampleText, pattern);

        Console.WriteLine(containsPattern);  // Utdata: True
    }
}
```

### Extrahera Data
Att extrahera data från en sträng med hjälp av grupper i ett regex kan göras med metoden `Regex.Match`.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Datum: 2023-04-12";
        string pattern = @"Datum: (\d{4})-(\d{2})-(\d{2})";
        Match match = Regex.Match(sampleText, pattern);

        if (match.Success)
        {
            Console.WriteLine($"År: {match.Groups[1].Value}");  // Utdata: År: 2023
            Console.WriteLine($"Månad: {match.Groups[2].Value}");  // Utdata: Månad: 04
            Console.WriteLine($"Dag: {match.Groups[3].Value}");  // Utdata: Dag: 12
        }
    }
}
```

### Ersätta Text
Metoden `Regex.Replace` låter dig ersätta text i en sträng som matchar ett angivet mönster.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Besök Microsoft!";
        string pattern = "Microsoft";
        string replacement = "Google";

        string result = Regex.Replace(sampleText, pattern, replacement);

        Console.WriteLine(result);  // Utdata: Besök Google!
    }
}
```

### Dela upp Strängar
Du kan dela upp en sträng i en array baserad på ett regex-mönster med metoden `Regex.Split`.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "ett,två,tre,fyra,fem";
        string pattern = ",";

        string[] result = Regex.Split(sampleText, pattern);

        foreach (string item in result)
        {
            Console.WriteLine(item);
        }
        // Utdata: 
        // ett
        // två
        // tre
        // fyra
        // fem
    }
}
```

### Användning av Tredjepartsbibliotek
Även om .NET Framework erbjuder omfattande stöd för reguljära uttryck, finns det också tredjepartsbibliotek såsom `PCRE.NET` som erbjuder Perl-kompatibla reguljära uttryck (PCRE) i C#. Detta kan vara användbart om du behöver funktioner eller syntax från Pearls regex-motor som inte är tillgängliga i .NET:s implementation.

För att använda `PCRE.NET`, skulle du först installera dess NuGet-paket, och sedan kan du använda det på ett liknande sätt som du använder de inbyggda .NET regex-klasserna.

```csharp
// Exempel med PCRE.NET här
// Not: Föreställ dig ett exempel liknande dem ovan, anpassat för att visa en funktion unik för PCRE.NET.
```

När du integrerar tredjepartsbibliotek för reguljära uttryck, konsultera alltid deras dokumentation för detaljerad användnings- och kompatibilitetsinformation.
