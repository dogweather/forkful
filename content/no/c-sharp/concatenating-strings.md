---
title:                "Sammenslåing av strenger"
date:                  2024-01-20T17:34:15.048974-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sammenslåing av strenger"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (Hva & Hvorfor?)
Sammenkjeding av strenger i C# er det å slå sammen tekst. Det lar deg raskt sette sammen variabler, input og statisk tekst for utskrift, lagring eller videre behandling.

## How to: (Slik gjør du det:)
```C#
string hello = "Hei ";
string world = "verden!";
string greeting = hello + world; // Vanlig sammenkjedning

// Med String.Concat()
string fullGreeting = String.Concat(hello, world); // "Hei verden!"

// Med '$' - string interpolasjon (anbefalt for lesbarhet)
string interpolatedGreeting = $"{hello}{world}"; // "Hei verden!"

Console.WriteLine(greeting); // Utskrift: Hei verden!
Console.WriteLine(fullGreeting); // Utskrift: Hei verden!
Console.WriteLine(interpolatedGreeting); // Utskrift: Hei verden!
```

## Deep Dive (Dypdykk)
I C#'s historie har flere metoder for å sammenkoble strenger dukket opp. '+'-operatoren er grei, men kan bli tungvint med mange variabler. `String.Concat()` er mer eksplisitt og effektiv med flere strenger. String interpolasjon, introdusert i C# 6.0, gjør koden mer lesbar og skaper mindre forvirring ved sammensatte uttrykk.

Alternativt, hvis du arbeider med store mengder tekst eller hyppige operasjoner, bør du bruke `StringBuilder` som er optimalisert for slike oppgaver. 

Implementeringen av strengsammensetninger i .NET bruker intern optimalisering for å håndtere minnebruk effektivt, men overdreven bruk kan likevel føre til unødvendig høy belasting på garbage collector.

## See Also (Se også)
- [String interpolation in C#](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated)
- [StringBuilder Class](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder?view=net-6.0)
- [String.Concat Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.concat?view=net-6.0)
