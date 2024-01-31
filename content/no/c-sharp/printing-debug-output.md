---
title:                "Skrive ut feilsøkingsdata"
date:                  2024-01-20T17:52:04.615061-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skrive ut feilsøkingsdata"

category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (Hva & Hvorfor?)
Skrive ut feilsøkingsdata hjelper utviklere å se hva som foregår under panseret av programmene sine. Vi gjør det for å fakke bugs og forstå flyten, uten å granske tusenvis av kodelinjer.

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
