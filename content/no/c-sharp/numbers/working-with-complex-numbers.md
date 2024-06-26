---
date: 2024-01-26 04:38:19.282988-07:00
description: 'Hvordan: C# har en innebygd `System.Numerics.Complex` struktur for behandling
  av komplekse tall. Her er en rask gjennomgang.'
lastmod: '2024-03-13T22:44:40.789095-06:00'
model: gpt-4-0125-preview
summary: C# har en innebygd `System.Numerics.Complex` struktur for behandling av komplekse
  tall.
title: "\xC5 jobbe med komplekse tall"
weight: 14
---

## Hvordan:
C# har en innebygd `System.Numerics.Complex` struktur for behandling av komplekse tall. Her er en rask gjennomgang:

```C#
using System;
using System.Numerics;

class EksempelKomplekseTall
{
    static void Main()
    {
        // Å opprette komplekse tall
        Complex c1 = new Complex(4, 5); // 4 + 5i
        Complex c2 = Complex.FromPolarCoordinates(1, Math.PI / 4); // 1 * e^(iπ/4)

        // Grunnleggende operasjoner
        Complex sum = c1 + c2;
        Complex differanse = c1 - c2;
        Complex produkt = c1 * c2;
        Complex kvotient = c1 / c2;

        // Skrive ut resultater
        Console.WriteLine($"Sum: {sum}");
        Console.WriteLine($"Differanse: {differanse}");
        Console.WriteLine($"Produkt: {produkt}");
        Console.WriteLine($"Kvotient: {kvotient}");
        Console.WriteLine($"Magnitude av c1: {c1.Magnitude}");
        Console.WriteLine($"Fase av c1: {c1.Phase}");
    }
}
```

Og det vil skrive ut:

```
Sum: (4.70710678118655, 5.70710678118655)
Differanse: (3.29289321881345, 4.29289321881345)
Produkt: (-1.00000000000001, 9)
Kvotient: (0.6, 0.8)
Magnitude av c1: 6.40312423743285
Fase av c1: 0.896055384571344
```

## Dypdykk
Komplekse tall, bestående av en reel og en imaginær del (ofte notert som a + bi), har eksistert siden det 17. århundret. Den italienske matematikeren Gerolamo Cardano krediteres for deres tidlige utvikling. I programmering innebærer håndtering av komplekse tall å forstå og håndtere disse to distinkte delene.

Mens C#'s `System.Numerics.Complex` er robust og integrert i språket, tilbyr andre språk som Python lignende funksjonalitet med `cmath` eller tredjepartsbiblioteker. Og hvis du jobber i en eldre versjon av C# eller en .NET-versjon som ikke støtter `System.Numerics`, må du kanskje lage din egen klasse for komplekse tall eller finne et bibliotek.

Internt bruker operasjonene på komplekse tall flyttallsaritmetikk, som kan introdusere avrundingsfeil. Så når du implementerer algoritmer som bruker komplekse tall omfattende, er det viktig å huske på dette og vurdere innvirkningen på presisjon og nøyaktighet.

## Se Også
1. C# Referanse for `System.Numerics.Complex`: https://learn.microsoft.com/en-us/dotnet/api/system.numerics.complex
2. Et dypere dykk i matematikken til komplekse tall: https://mathworld.wolfram.com/ComplexNumber.html
3. For alternative implementeringer og biblioteker, sjekk ut Math.NET Numerics: https://numerics.mathdotnet.com/
