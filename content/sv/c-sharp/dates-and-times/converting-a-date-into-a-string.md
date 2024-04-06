---
date: 2024-01-20 17:36:09.684219-07:00
description: "Hur g\xF6r man: Historiskt sett har behovet av att konvertera datum\
  \ till str\xE4ngar funnits lika l\xE4nge som behovet av m\xE4nsklig kommunikation\
  \ av datum. I C#,\u2026"
lastmod: '2024-04-05T22:50:52.223672-06:00'
model: gpt-4-1106-preview
summary: "Historiskt sett har behovet av att konvertera datum till str\xE4ngar funnits\
  \ lika l\xE4nge som behovet av m\xE4nsklig kommunikation av datum."
title: "Omvandla ett datum till en str\xE4ng"
weight: 28
---

## Hur gör man:
```
C#
DateTime nu = DateTime.Now;
string datumSomString = nu.ToString("yyyy-MM-dd");
Console.WriteLine(datumSomString);
```
Output:
```
2023-04-02
```
För att anpassa formatet efter svenska förhållanden, använd "yyyy-MM-dd" som formatsträng. Detta motsvarar ISO 8601, som är standard i Sverige.

## Fördjupning
Historiskt sett har behovet av att konvertera datum till strängar funnits lika länge som behovet av mänsklig kommunikation av datum. I C#, `ToString` metoden på `DateTime` objekt har varit hjärtat av denna konvertering sedan .NET's början. Alternativ för formatsträng innehåller fördefinierade standardalternativ som 'd' för kort datumformat eller 'T' för fullständig tid. För mer kontroll används egna formatsträngar, precis som exemplet ovan.

I vissa fall när prestanda är kritiskt, kan `StringBuilder` eller `Span<T>` användas för ytterligare optimeringar. Det är också möjligt att använda tredjepartsbibliotek som `NodaTime` för mer komplex hantering av datum och tider.

## Se även
- [Officiell dokumentation för DateTime.ToString Method](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring)
- [Microsofts Översikt över Standard Datum och Tid Formatsträngar](https://docs.microsoft.com/en-us/dotnet/standard/base-types/standard-date-and-time-format-strings)
- [Microsofts Översikt över Anpassade Datum och Tid Formatsträngar](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [NodaTime dokumentation](https://nodatime.org/)
