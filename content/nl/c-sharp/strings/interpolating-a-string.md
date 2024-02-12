---
title:                "Een string interpoleren"
aliases: - /nl/c-sharp/interpolating-a-string.md
date:                  2024-01-28T22:02:01.050396-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een string interpoleren"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c-sharp/interpolating-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
String interpolatie stelt je in staat om strings te bouwen met behulp van ingesloten expressies. Het maakt code leesbaar en formatteren een fluitje van een cent.

## Hoe te:
```C#
string naam = "Alex";
int leeftijd = 29;
string groet = $"Hallo, {naam}! Je bent {leeftijd} jaar oud.";
Console.WriteLine(groet);
```
Uitvoer:
```
Hallo, Alex! Je bent 29 jaar oud.
```

## Diepgaande Duik
String interpolatie is geïntroduceerd in C# 6, wat het gemak van het formatteren van strings versterkt ten opzichte van de oudere `String.Format` methode. Historisch gezien zou je zoiets als dit kunnen tegenkomen:

```C#
string groet = string.Format("Hallo, {0}! Je bent {1} jaar oud.", naam, leeftijd);
```

Interpolatie in C# is een syntactische suiker die de compiler omzet in een `String.Format` aanroep. Het werkt door de geïnterpoleerde string te parsen en de expressies ingesloten in `{}` te vervangen door de stringrepresentaties van de resultaten van de expressies. Intern gebruikt het een `StringBuilder` onder de motorkap, waardoor het efficiënter is dan concatenatie in loops.

Een alternatief voor string interpolatie is de plus (`+`) operator voor concatenatie, maar dat kan snel onleesbaar en omslachtig worden, en vaak meer foutgevoelig.

```C#
string groet = "Hallo, " + naam + "! Je bent " + leeftijd + " jaar oud.";
```

Gezien deze alternatieven is string interpolatie vaak de voorkeurskeuze vanwege de duidelijkheid en efficiëntie in de meeste scenario's.

## Zie Ook
Voor meer over string formattering in C#, MSDN is je maatje:
- [String interpolatie](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated)
- [String.Format](https://docs.microsoft.com/en-us/dotnet/api/system.string.format?view=net-6.0)
- [StringBuilder](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder?view=net-6.0)
