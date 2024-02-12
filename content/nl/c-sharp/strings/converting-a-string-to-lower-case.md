---
title:                "Een string omzetten naar kleine letters"
aliases:
- /nl/c-sharp/converting-a-string-to-lower-case.md
date:                  2024-01-28T21:57:46.445048-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een string omzetten naar kleine letters"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c-sharp/converting-a-string-to-lower-case.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een tekenreeks converteren naar kleine letters betekent dat alle hoofdletters in de tekst worden getransformeerd naar hun kleine letter equivalenten. Programmeurs doen dit voor consistentie, vooral voor hoofdletterongevoelige vergelijkingen, zoeken, of om data-inputs te standaardiseren voor verwerking.

## Hoe:
In C# kun je een tekenreeks converteren naar kleine letters met behulp van de `ToLower()` of `ToLowerInvariant()` methoden. Hier is hoe:

```C#
string originalText = "Hallo, Wereld!";
string lowerCaseText = originalText.ToLower();

Console.WriteLine(lowerCaseText); // Print: hallo, wereld!
```

En voor cultuur-onafhankelijke conversies:

```C#
string mixedCaseText = "İstanbul";
string lowerInvariantText = mixedCaseText.ToLowerInvariant();

Console.WriteLine(lowerInvariantText); // Print: i̇stanbul
```

Voorbeelduitvoer:

```
hallo, wereld!
i̇stanbul
```

## Diepere Duik
Historisch gezien komt de behoefte om strings naar kleine letters om te zetten van computersystemen die begonnen met hoofdletterongevoelige commando's. Tegenwoordig doen we dit nog steeds om drie hoofdredenen:

1. **Consistentie**: Bij het behandelen van input, vooral door gebruikers gegenereerde data, zorgt het converteren naar kleine letters voor een gestandaardiseerd formaat.
2. **Hoofdletterongevoelige Operaties**: Dit omvat het zoeken, sorteren en vergelijken van tekenreeksen waarbij "Appel" hetzelfde moet worden behandeld als "appel".
3. **Lokalisatie**: Talen hebben verschillende regels voor hoofd- en kleine letters. `ToLowerInvariant()` pakt dit aan door een cultuur-onafhankelijke conversie te bieden, die karakters naar kleine letters verandert op basis van een invariante cultuur (vergelijkbaar met Engels) en onverwachte resultaten voorkomt.

Alternatieven voor `.ToLower()` en `.ToLowerInvariant()` zijn onder meer het gebruik van reguliere expressies voor vervangingen of het handmatig doorlopen van een tekenreeks voor aangepaste conversiescenario's.

Implementatie-detailgewijs, deze methoden wijzigen de oorspronkelijke tekenreeks niet; tekenreeksen in .NET zijn onveranderlijk. Ze creëren en retourneren een nieuwe tekenreeks die de kleine letterversie is van het origineel.

## Zie Ook
- Tekenreeksklasse in C# Documentatie: [Microsoft Docs](https://docs.microsoft.com/nl-nl/dotnet/api/system.string)
- StringComparison Enum en Cultuur-Onafhankelijke Vergelijkingen: [Microsoft Docs](https://docs.microsoft.com/nl-nl/dotnet/standard/base-types/best-practices-strings)
