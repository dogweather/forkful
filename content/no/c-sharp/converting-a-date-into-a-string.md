---
title:    "C#: Konvertere dato til streng"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Hvorfor
Det å konvertere en dato til en streng er en viktig ferdighet å ha som programmerer, spesielt når du jobber med applikasjoner som involverer store mengder datoer. Ved å konvertere en dato til en streng kan du gjøre den mer leselig og mer tilpasset til dine spesifikke behov.

## Hvordan
Det er flere måter å konvertere en dato til en streng i C#. En vanlig måte er å bruke metoden `ToString()` som finnes på `DateTime` objektet. Her er et eksempel på hvordan man kan bruke denne metoden:
```C#
DateTime today = DateTime.Today;
Console.WriteLine(today.ToString("dd/MM/yyyy"));
```
Dette vil gi følgende output: 27/08/2021. Legg merke til at du kan tilpasse strengen ved å bruke forskjellige formateringsalternativer som "dd" for dag, "MM" for måned og "yyyy" for år.

En annen metode for konvertering er å bruke `string.Format()` funksjonen. Her er et eksempel på hvordan man kan bruke denne metoden:
```C#
DateTime date = new DateTime(2021, 08, 27);
string formattedDate = string.Format("{0:yyyy-MM-dd}", date);
Console.WriteLine(formattedDate);
```
Dette vil gi følgende output: 2021-08-27. Igjen, du kan tilpasse formateringen ved å endre på formatstrengen.

## Dykke dypere
Når du bruker metoden `ToString()` er det viktig å merke seg at den gjør en implisitt konvertering av datoverdien til klokkeslettets tidsone. Dette kan føre til uventede resultater hvis du ikke er klar over det. For å unngå dette, kan du bruke `DateTimeOffset` objektet og den tilsvarende `ToString()` metoden som tar en `IFormatProvider` som et argument. Dette vil resultere i at datoen blir konvertert til den lokale tidszonen fremfor klokkeslettets tidsone.

## Se også
- [DateTime.ToString() Metode (System)](https://docs.microsoft.com/nb-no/dotnet/api/system.datetime.tostring?view=net-5.0) 
- [String.Format Metode (System)](https://docs.microsoft.com/nb-no/dotnet/api/system.string.format?view=net-5.0)