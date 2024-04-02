---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:20.221296-07:00
description: "Het omzetten van een datum naar een tekenreeks in C# houdt in dat het\
  \ formaat van een DateTime-object wordt veranderd naar een tekstweergave. Programmeurs\u2026"
lastmod: '2024-03-13T22:44:50.821479-06:00'
model: gpt-4-0125-preview
summary: "Het omzetten van een datum naar een tekenreeks in C# houdt in dat het formaat\
  \ van een DateTime-object wordt veranderd naar een tekstweergave. Programmeurs\u2026"
title: Een datum converteren naar een string
weight: 28
---

## Wat & Waarom?

Het omzetten van een datum naar een tekenreeks in C# houdt in dat het formaat van een DateTime-object wordt veranderd naar een tekstweergave. Programmeurs doen dit om data weer te geven in een gebruiksvriendelijk formaat of om de gegevens te serialiseren voor opslag en overdracht.

## Hoe te:

In C# heb je het `DateTime`-object en een heleboel manieren om het om te zetten in een tekenreeks. Hier zijn er een paar:

```csharp
DateTime now = DateTime.Now;
string defaultString = now.ToString(); // Standaardformaat
string specificFormat = now.ToString("yyyy-MM-dd"); // Aangepast formaat, hier ISO 8601
string withCulture = now.ToString("d", new CultureInfo("en-US")); // Korte datum in de cultuur van de VS

Console.WriteLine(defaultString); // Uitvoer hangt af van systeemcultuurinstellingen
Console.WriteLine(specificFormat); // Uitvoer: "2023-04-01"
Console.WriteLine(withCulture); // Uitvoer: "4/1/2023"
```

## Diepgaand

Vroeger was het manipuleren van data en tekenreeksen lastiger. Vandaag de dag biedt C#'s `DateTime` `.ToString()` met overloads voor cultuur en formaat. De `IFormatProvider` interface, zoals `CultureInfo`, regelt cultuurspecifieke opmaak.

Alternatieven? Zeker! `String.Format` en interpolatie (`$"{now:yyyy-MM-dd}"`) zijn opties voor het invoegen van datums in tekenreeksen met context. `DateTimeOffset` is handig voor tijdzonespecificaties.

Wat betreft de implementatie, onthoud dat `DateTime` een struct is, dus een waarde type. Het omzetten ervan verandert het origineel niet: onveranderlijkheid voor de winst. Kies je tekenreeksformaat verstandig, afhankelijk van je publiek (eindgebruikers) en het systeem waarmee je interfaceert (databases, API's).

## Zie Ook

- [DateTime.ToString Methode](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring)
- [Aangepaste datum- en tijdformaattekenreeksen](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [CultureInfo Klasse](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo)
