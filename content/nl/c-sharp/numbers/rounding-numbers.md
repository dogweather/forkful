---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:49.682790-07:00
description: "Afronden van getallen betekent het aanpassen ervan naar de dichtstbijzijnde\
  \ gespecificeerde plaatswaarde - denk aan het terugbrengen tot een eenvoudigere\u2026"
lastmod: '2024-02-25T18:49:48.144513-07:00'
model: gpt-4-0125-preview
summary: "Afronden van getallen betekent het aanpassen ervan naar de dichtstbijzijnde\
  \ gespecificeerde plaatswaarde - denk aan het terugbrengen tot een eenvoudigere\u2026"
title: Afronden van getallen
---

{{< edit_this_page >}}

## Wat & Waarom?
Afronden van getallen betekent het aanpassen ervan naar de dichtstbijzijnde gespecificeerde plaatswaarde - denk aan het terugbrengen tot een eenvoudigere vorm. Programmeurs ronden af om precisie te beheersen, prestaties te verbeteren of wanneer ze gebruikersvriendelijke resultaten tonen - zoals prijzen die geen drie decimalen nodig hebben.

## Hoe te:
Hier is je retourticket voor het afronden van getallen in C#:

```csharp
using System;

public class VoorbeeldAfronden
{
    public static void Main()
    {
        double origineelNummer = 123.4567;

        // Afronden naar het dichtstbijzijnde gehele getal
        double afgerond = Math.Round(origineelNummer);
        Console.WriteLine(afgerond); // Uitvoer: 123

        // Aantal decimalen specificeren
        double afgerondTweeDecimalen = Math.Round(origineelNummer, 2);
        Console.WriteLine(afgerondTweeDecimalen); // Uitvoer: 123.46

        // Afronden naar boven ongeacht de volgende cijfer
        double naarBovenAfgerond = Math.Ceiling(origineelNummer);
        Console.WriteLine(naarBovenAfgerond); // Uitvoer: 124

        // Afronden naar beneden ongeacht de volgende cijfer
        double naarBenedenAfgerond = Math.Floor(origineelNummer);
        Console.WriteLine(naarBenedenAfgerond); // Uitvoer: 123
    }
}
```

## Diepgaande Duik
Vroeger was afronden een fluitje van een cent om de computationele kosten te drukken. Elke cyclus telde, en het bijsnijden van getallen bespaarde kostbare tijd. Ga snel vooruit naar modern C#, en het gaat om het beheersen van de beruchte neiging tot precisiefouten en weergave eigenaardigheden van doubles en decimals.

Voorbij `Math.Round`, `Math.Floor` en `Math.Ceiling`, laat de `MidpointRounding` enum ons het lot van de arme, in het midden zittende cijfers bepalen - het is het kruispunt tussen bankregels en de speeltuinrechtvaardigheid van "rond de helft naar boven af".

Voor de lastigere menigten, zoals serieuze wiskunde of financiële toepassingen, hebben we `decimal` over `double`, het vermindert afrondingsdrama door hogere precisie te bieden - minder afronden, minder problemen.

## Zie Ook
- [Officiële C# Documentatie over `Math.Round`](https://docs.microsoft.com/en-us/dotnet/api/system.math.round)
- [Stack Overflow: Wanneer moet ik Double gebruiken in plaats van Decimal?](https://stackoverflow.com/questions/1165761/decimal-vs-double-which-one-should-i-use-and-when)
- [IEEE Norm voor Zwevende-kommagetallen (IEEE 754)](https://nl.wikipedia.org/wiki/IEEE_754)
