---
date: 2024-01-20 17:45:29.679321-07:00
description: "How to: (Slik gj\xF8r du) I C# har du hatt muligheten til \xE5 trekke\
  \ ut delstrenger siden de f\xF8rste versjonene. Substring-metoden er en del av System.String-\u2026"
lastmod: '2024-04-05T22:50:54.793690-06:00'
model: gpt-4-1106-preview
summary: "(Slik gj\xF8r du) I C# har du hatt muligheten til \xE5 trekke ut delstrenger\
  \ siden de f\xF8rste versjonene."
title: Uthenting av delstrenger
weight: 6
---

## How to: (Slik gjør du)
```C#
string fullText = "Hallo, Norge!";
string extracted = fullText.Substring(7, 5); // Starter ved indeks 7, lengde 5

Console.WriteLine(extracted); // Output: Norge
```
```C#
string birthYear = "Fødselsår: 1984";
string year = birthYear.Substring(11); // Starter ved indeks 11 til slutten

Console.WriteLine(year); // Output: 1984
```
```C#
// Får en feilmelding hvis indeksen eller lengden overskrider strengens grenser. Pass på dette!
```

## Deep Dive (Dypdykk)
I C# har du hatt muligheten til å trekke ut delstrenger siden de første versjonene. Substring-metoden er en del av System.String-klassen. Alternativer inkluderer bruk av `Span<T>` for å unngå unødvendig kopiering av strenger i minnet, noe som er nyttig for ytelse. Implementeringsmessig bruker Substring interne funksjoner til .NET for å håndtere minnehåndtering og sikre sikkerhet ved tilgang til strenger.

## See Also (Se Også)
- String manipulation in C# ([https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/))
- .NET API documentation for String.Substring Method ([https://docs.microsoft.com/en-us/dotnet/api/system.string.substring](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring))
- Performance tips for C# ([https://docs.microsoft.com/en-us/dotnet/csharp/write-safe-efficient-code](https://docs.microsoft.com/en-us/dotnet/csharp/write-safe-efficient-code))
