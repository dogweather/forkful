---
title:                "Een datum converteren naar een string"
date:                  2024-01-28T21:57:20.221296-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een datum converteren naar een string"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c-sharp/converting-a-date-into-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
