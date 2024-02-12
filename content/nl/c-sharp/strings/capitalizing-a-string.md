---
title:                "Een string met hoofdletters maken"
aliases: - /nl/c-sharp/capitalizing-a-string.md
date:                  2024-01-28T21:55:40.393493-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een string met hoofdletters maken"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c-sharp/capitalizing-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een tekenreeks kapitaliseren in programmeren betekent alle letters in een tekenreeks in hoofdletters omzetten. Het is een vaak voorkomende taak voor het formatteren van uitvoer, het verbeteren van de leesbaarheid, of het voorbereiden van gegevens voor vergelijking of opslagconsistentie.

## Hoe:

In C# kan je een tekenreeks kapitaliseren met de `ToUpper` methode op een tekenreeksinstantie. Zo ziet het eruit:

```C#
string origineel = "hallo wereld!";
string gekapitaliseerd = origineel.ToUpper();

Console.WriteLine(gekapitaliseerd); // Uitvoer: HALLO WERELD!
```

Zo simpel is het - je tekenreeks schreeuwt nu naar je in alle hoofdletters.

## Diepere Duik

Kapitaliseren is geen moderne uitvinding. Oude manuscripten begonnen vaak met grote, decoratieve capitulum, of hoofdletters. Snel vooruit naar de informatica: kapitaliseren dient praktische rollen, zoals het laten opvallen van titels of het waarborgen van hoofdletterongevoelige vergelijkingen.

Hoewel `.ToUpper()` eenvoudig is, let op de alternatieven en eigenaardigheden:

1. **Cultuurgevoeligheid**: Standaard gebruikt `ToUpper()` de hoofdletterregels van de huidige cultuur. Als je een cultuur-onafhankelijk resultaat nodig hebt, gebruik dan `ToUpperInvariant()`.

2. **Prestatie**: Het herhaaldelijk kapitaliseren van tekenreeksen kan kostbaar zijn, vooral in lussen. Let op voor onnodige conversies.

3. **Alternatieven**: Er is ook `ToLower()`, voor het tegenovergestelde effect (een tekenreeks geheel in kleine letters maken), en `TextInfo.ToTitleCase()`, voor het kapitaliseren van slechts de eerste letter van elk woord.

4. **Beveiligingspraktijken**: Wees voorzichtig met transformaties met beveiligingsimplicaties. Bijvoorbeeld, wachtwoordvergelijkingen moeten altijd hoofdlettergevoelig zijn om complexiteit te behouden.

Zo kapitaliseer je terwijl je cultuur-onafhankelijk bent:

```C#
string origineel = "iççe";
string gekapitaliseerdInvariant = origineel.ToUpperInvariant();

Console.WriteLine(gekapitaliseerdInvariant); // Uitvoer: İÇÇE
```

Merk op dat de punt over de 'i' blijft na het kapitaliseren volgens de regels van de invariant cultuur.

## Zie Ook:

- Officiële documentatie van Microsoft over `.ToUpper()`:
  [MSDN - String.ToUpper Methode](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper)
  
- Introductie tot CultureInfo:
  [MSDN - CultureInfo Klasse](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo)

- Beste praktijken voor het gebruik van tekenreeksen in .NET:
  [MSDN - Beste Praktijken voor het Gebruik van Tekenreeksen in .NET](https://docs.microsoft.com/en-us/dotnet/standard/base-types/best-practices-strings)
