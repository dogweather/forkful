---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:27.551915-07:00
description: "Wie: Um zu \xFCberpr\xFCfen, ob eine Zeichenkette ein spezifisches Muster\
  \ enth\xE4lt, k\xF6nnen Sie die `Regex.IsMatch` Methode aus dem\u2026"
lastmod: '2024-03-13T22:44:53.878274-06:00'
model: gpt-4-0125-preview
summary: "Um zu \xFCberpr\xFCfen, ob eine Zeichenkette ein spezifisches Muster enth\xE4\
  lt, k\xF6nnen Sie die `Regex.IsMatch` Methode aus dem `System.Text.RegularExpressions`\
  \ Namensraum nutzen."
title: "Regul\xE4re Ausdr\xFCcke verwenden"
weight: 11
---

## Wie:


### Einfaches Mustervergleichen
Um zu überprüfen, ob eine Zeichenkette ein spezifisches Muster enthält, können Sie die `Regex.IsMatch` Methode aus dem `System.Text.RegularExpressions` Namensraum nutzen.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Hallo, Welt!";
        string pattern = "Welt";
        bool containsPattern = Regex.IsMatch(sampleText, pattern);

        Console.WriteLine(containsPattern);  // Ausgabe: True
    }
}
```

### Daten extrahieren
Das Extrahieren von Daten aus einer Zeichenkette mit Gruppen in einem regex kann mit der `Regex.Match` Methode durchgeführt werden.

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
            Console.WriteLine($"Jahr: {match.Groups[1].Value}");  // Ausgabe: Jahr: 2023
            Console.WriteLine($"Monat: {match.Groups[2].Value}");  // Ausgabe: Monat: 04
            Console.WriteLine($"Tag: {match.Groups[3].Value}");  // Ausgabe: Tag: 12
        }
    }
}
```

### Text ersetzen
Die `Regex.Replace` Methode ermöglicht es Ihnen, Text in einer Zeichenkette, der einem spezifizierten Muster entspricht, zu ersetzen.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Besuche Microsoft!";
        string pattern = "Microsoft";
        string replacement = "Google";

        string result = Regex.Replace(sampleText, pattern, replacement);

        Console.WriteLine(result);  // Ausgabe: Besuche Google!
    }
}
```

### Zeichenketten aufteilen
Sie können eine Zeichenkette basierend auf einem regex Muster mit der `Regex.Split` Methode in ein Array aufteilen.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "eins,zwei,drei,vier,fünf";
        string pattern = ",";

        string[] result = Regex.Split(sampleText, pattern);

        foreach (string item in result)
        {
            Console.WriteLine(item);
        }
        // Ausgabe: 
        // eins
        // zwei
        // drei
        // vier
        // fünf
    }
}
```

### Nutzung von Drittanbieter-Bibliotheken
Obwohl das .NET Framework umfassende Unterstützung für reguläre Ausdrücke bietet, gibt es auch Drittanbieter-Bibliotheken wie `PCRE.NET`, die Perl-kompatible reguläre Ausdrücke (PCRE) in C# anbieten. Dies kann nützlich sein, wenn Sie Funktionen oder eine Syntax aus Perls Regex-Engine benötigen, die in der .NET-Implementierung nicht verfügbar sind.

Um `PCRE.NET` zu nutzen, würden Sie zunächst sein NuGet-Paket installieren, und dann können Sie es ähnlich verwenden, wie Sie die nativen .NET regex Klassen nutzen.

```csharp
// Beispiel mit PCRE.NET hier
// Hinweis: Stellen Sie sich ein Beispiel vor, ähnlich den oben genannten, zugeschnitten darauf, ein einzigartiges Feature von PCRE.NET zu zeigen.
```

Bei der Integration von Drittanbieter-Bibliotheken für reguläre Ausdrücke konsultieren Sie immer deren Dokumentation für detaillierte Nutzung- und Kompatibilitätsinformationen.
