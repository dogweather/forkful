---
title:                "Beregning av en dato i fremtiden eller fortiden"
html_title:           "C#: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Fram og Tilbake i Tiden med C#

## Hva & Hvorfor?
Beregning av en dato i fremtiden eller fortiden er prosessen med å legge til eller trekke fra et bestemt tidsrom til en spesifikk dato. Programmere gjør dette for å håndtere dato/tid relaterte operasjoner i forskjellige applikasjoner som kalendere, planleggere, påminnelser, osv.

## Hvordan Gjøre:
Her er et enkelt eksempel på hvordan du kan beregne en fremtidig dato med C#:

```C#
DateTime nå = DateTime.Now;
DateTime fremtid = nå.AddDays(7);
Console.WriteLine("Datoen om 7 dager vil være: " + fremtid.ToShortDateString());
```
Dette vil gi en utskrift som:
```
Datoen om 7 dager vil være: DD.MM.YYYY
```
For å beregne en dato i fortiden, kan vi følge samme mønster:

```C#
DateTime nå = DateTime.Now;
DateTime fortid = nå.AddDays(-7);
Console.WriteLine("Datoen for 7 dager siden var: " + fortid.ToShortDateString());
```
Dette vil gi en utskrift som:
```
Datoen for 7 dager siden var: DD.MM.YYYY
```

## Dyp Dykk
Beregning av en dato i fortiden eller fremtiden er en eldgammel praksis som strekker seg langt ut over den digitale tidsalderen. I C# kan vi enkelt justere en dato med `AddDays()`, `AddMonths()`, eller `AddYears()` funksjoner. 

Alternativt kan du også bruke `TimeSpan` strukturen, noe som gir mer nøyaktig kontroll over perioden du vil legge til eller trekke fra.

Her er et eksempel på bruk av `TimeSpan`:
```C#
DateTime nå = DateTime.Now;
TimeSpan syvDager = new TimeSpan(7, 0, 0, 0);
DateTime fremtid = nå.Add(syvDager);
Console.WriteLine("Datoen om 7 dager vil være: " + fremtid.ToShortDateString());
```

For eventuelle tidsjusteringer, er det viktig å håndtere tidssone konverteringer, spesielt for applikasjoner som vil bli brukt på tvers av forskjellige tidssoner.

## Se Også
Dokumentasjon for [`DateTime.Add`](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.add)

Dokumentasjon for [`TimeSpan`](https://docs.microsoft.com/en-us/dotnet/api/system.timespan)

Howard’s forklaring om [Beregning av datoer i C#](https://www.essentialsql.com/calculate-dates-csharp/)