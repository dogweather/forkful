---
title:                "Karakters verwijderen die overeenkomen met een patroon"
aliases:
- nl/c-sharp/deleting-characters-matching-a-pattern.md
date:                  2024-01-28T21:58:24.800564-07:00
model:                 gpt-4-0125-preview
simple_title:         "Karakters verwijderen die overeenkomen met een patroon"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c-sharp/deleting-characters-matching-a-pattern.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het verwijderen van tekens die overeenkomen met een patroon gaat over het vinden en verwijderen van specifieke reeksen tekens uit strings op basis van regels (zoals regex). Programmeurs doen dit om gegevens op te schonen, invoer te valideren of tekst te manipuleren voor diverse doeleinden.

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
