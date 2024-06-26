---
date: 2024-01-20 17:52:04.615061-07:00
description: "How to: (Hvordan:) Bruk Console.WriteLine for \xE5 skrive ut verdier\
  \ under kj\xF8ring. Ganske greit."
lastmod: '2024-04-05T21:53:41.772928-06:00'
model: gpt-4-1106-preview
summary: "(Hvordan:) Bruk Console.WriteLine for \xE5 skrive ut verdier under kj\xF8\
  ring."
title: "Skrive ut feils\xF8kingsdata"
weight: 33
---

## How to: (Hvordan:)
```C#
using System;

class Program
{
    static void Main()
    {
        Console.WriteLine("Start debugging...");
        
        int value = DebugValue();
        Console.WriteLine($"Debug Value: {value}");
        
        // More code here
        
        Console.WriteLine("End debugging.");
    }
    
    static int DebugValue()
    {
        int temp = 42; // La oss anta dette er en interessant verdi
        Console.WriteLine($"In DebugValue, temp is {temp}");
        return temp;
    }
}

// Sample output:
// Start debugging...
// In DebugValue, temp is 42
// Debug Value: 42
// End debugging.
```
Bruk Console.WriteLine for å skrive ut verdier under kjøring. Ganske greit.

## Deep Dive (Dypdykk)
Før, da alt var tekstbasert, var utskrift til konsoll standard for feilsøking. Alternativer inkluderer bruk av debuggere eller logger med nivåer som info, warn og error. Beskrivelser i koden (som `// La oss anta dette er en interessant verdi`) er også nyttig. Ulempen er at `Console.WriteLine` kan gjøre koden rotete og langsom i produksjon, så husk å fjerne dem før lansering eller bruk betingelser for å kontrollere utskrift i produksjonsmiljø.

## See Also (Se Også)
- Microsoft C# dokumentasjon: [Console.WriteLine Method](https://docs.microsoft.com/en-us/dotnet/api/system.console.writeline)
- Blogginnlegg om logging i .NET: [Logging and tracing in .NET](https://docs.microsoft.com/en-us/aspnet/core/fundamentals/logging/)
