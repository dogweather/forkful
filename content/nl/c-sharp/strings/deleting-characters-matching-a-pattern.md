---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:24.800564-07:00
description: 'Hoe: Wil je sommige tekens kwijt? Zo doe je dat in C#.'
lastmod: '2024-03-13T22:44:50.794066-06:00'
model: gpt-4-0125-preview
summary: Wil je sommige tekens kwijt.
title: Karakters verwijderen die overeenkomen met een patroon
weight: 5
---

## Hoe:
Wil je sommige tekens kwijt? Zo doe je dat in C#:

```C#
using System;
using System.Text.RegularExpressions;

class PatroonVerwijdering
{
    static void Main()
    {
        string origineleTekst = "B4n4n4 P1zz4!";
        string patroon = @"[0-9]+"; // Verwijder alle cijfers
        
        string opgeschoondeTekst = Regex.Replace(origineleTekst, patroon, string.Empty);
        
        Console.WriteLine(opgeschoondeTekst); // Geeft uit: Bnnn Pzz!
    }
}
```
Moet 'a' gevolgd door een cijfer weg? Kijk dan eens:

```C#
string gerichteVerwijdering = "C4ndy C4ne";
string complexPatroon = @"a[0-9]"; // Richt zich op 'a' gevolgd door een cijfer

string verfijndeTekst = Regex.Replace(gerichteVerwijdering, complexPatroon, string.Empty);

Console.WriteLine(verfijndeTekst); // Geeft uit: Cndy Cne
```

## Uitdieping
Regex (Reguliere Expressies) voedt patroonpassingsmogelijkheden, teruggaand tot de theoretische roots in de jaren 1950 (dank, automa-theorie!). Alternatieven voor regex omvatten het directe gebruik van `String.Replace()` voor eenvoudigere vervangingen of aangepaste algoritmen als prestaties kritiek zijn (omdat regex wat overhead heeft). Deze alternatieven missen echter de flexibiliteit en precisie die regex biedt voor complexe patronen. Bij het implementeren van patroonverwijdering, wees bewust van de dubbelzijdige aard van regex – ze zijn krachtig maar kunnen cryptisch en traag zijn voor uitgebreide gegevens.

## Zie Ook
- Microsoft's Regex Documentatie: https://docs.microsoft.com/nl-nl/dotnet/standard/base-types/regular-expressions
- Regex101 (om regex patronen te testen): https://regex101.com/
- Introductie tot Automatentheorie: https://nl.wikipedia.org/wiki/Automatentheorie
