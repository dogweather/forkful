---
title:                "Twee datums vergelijken"
aliases:
- nl/c-sharp/comparing-two-dates.md
date:                  2024-01-28T21:56:40.819320-07:00
model:                 gpt-4-0125-preview
simple_title:         "Twee datums vergelijken"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c-sharp/comparing-two-dates.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het vergelijken van twee datums betekent controleren hoe ze zich tot elkaar verhouden—is de ene eerder, later of op het exacte zelfde moment als de andere. Programmeurs doen dit om planningen te beheren, leeftijdsverificatie, het triggeren van gebeurtenissen, en meer—basisch altijd wanneer we tijdverschillen moeten meten of gebeurtenissen in volgorde moeten zetten.

## Hoe te:

Laten we duiken in C# voor het vergelijken van datums. Stel we hebben twee `DateTime` objecten, `date1` en `date2`. We vergelijken ze met behulp van `DateTime.Compare(date1, date2)`, `date1.CompareTo(date2)`, of door eigenschappen rechtstreeks te vergelijken:

```C#
DateTime date1 = new DateTime(2023, 3, 25);
DateTime date2 = new DateTime(2023, 3, 30);

// Gebruikmakend van de statische methode DateTime.Compare
int resultaat = DateTime.Compare(date1, date2);

if(resultaat < 0)
    Console.WriteLine("date1 is eerder dan date2.");
else if(resultaat == 0)
    Console.WriteLine("date1 is hetzelfde als date2.");
else
    Console.WriteLine("date1 is later dan date2.");

// Gebruikmakend van de CompareTo instantiemethode
resultaat = date1.compareTo(date2);

if(resultaat < 0)
    Console.WriteLine("date1 is opnieuw eerder.");
else if(resultaat == 0)
    Console.WriteLine("Nog steeds dezelfde tijd?");
else
    Console.WriteLine("date1 lijkt deze keer later te zijn?");

// Directe vergelijking
if(date1 < date2)
    Console.WriteLine("Ja, date1 is eerder, dat zien we direct.");
else if(date1 == date2)
    Console.WriteLine("Gelijk, duidelijk en simpel.");
else
    Console.WriteLine("Of is date1 later? Nee, niet deze keer.");
```

Uitvoer zal tonen dat `date1` eerder is dan `date2` in alle vergelijkingen—je stelt het voor de hand liggende vast, maar dat is waar logs voor zijn.

## Diep Duiken

DateTime vergelijkingen zijn een onderdeel van C# sinds de introductie ervan, cruciaal voor het omgaan met het ooit belangrijke concept van tijd. Intern representeren `DateTime` waarden tikken sinds middernacht, 1 januari 0001, binnen de Common Language Runtime.

Snak je naar alternatieven? Je zou `TimeSpan` kunnen gebruiken voor verschillen, of het een niveau hoger tillen met NodaTime, een bibliotheek van Jon Skeet voor meer complexe behandeling van data en tijd.

Hier is een technisch leuk feitje: `DateTime`-typen in .NET kunnen `Unspecified`, `Utc`, of `Local` zijn. Een UTC-tijd vergelijken met een lokale tijd? Dat vraagt om problemen. Zorg altijd dat de typen overeenkomen om scheve logica te voorkomen!

## Zie Ook

Duik dieper of verduidelijk dingen met deze:

- Microsoft's DateTime documentatie: https://docs.microsoft.com/nl-nl/dotnet/api/system.datetime
- Meer over DateTime.Kind: https://docs.microsoft.com/nl-nl/dotnet/api/system.datetime.kind
- NodaTime, voor nieuwsgierige klok-kijkers: https://nodatime.org/
- TimeSpan voor tijdsverschillen: https://docs.microsoft.com/nl-nl/dotnet/api/system.timespan
