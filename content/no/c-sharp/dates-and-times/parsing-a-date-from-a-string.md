---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:48.803154-07:00
description: "Analysering av en dato fra en tekststreng i C# inneb\xE6rer konvertering\
  \ av tekstrepresentasjoner av datoer og tider til et `DateTime`-objekt. Dette er\u2026"
lastmod: '2024-03-13T22:44:40.806054-06:00'
model: gpt-4-0125-preview
summary: "Analysering av en dato fra en tekststreng i C# inneb\xE6rer konvertering\
  \ av tekstrepresentasjoner av datoer og tider til et `DateTime`-objekt."
title: Analysering av en dato fra en streng
weight: 30
---

## Hvordan:
**Grunnleggende Analysering:**

Metodene `DateTime.Parse` og `DateTime.TryParse` er de gå-til alternativene for å konvertere en tekststreng til en `DateTime`. Her er et kjapt eksempel:

```csharp
string dateString = "2023-04-12";
DateTime parsedDate;

if (DateTime.TryParse(dateString, out parsedDate))
{
    Console.WriteLine($"Vellykket analysert: {parsedDate}");
}
else
{
    Console.WriteLine("Klarte ikke å analysere.");
}
// Utdata: Vellykket analysert: 4/12/2023 12:00:00 AM
```

**Spesifisere en Kultur:**

Noen ganger trenger du å analysere en datostreng som er i et spesifikt kulturformat. Dette kan du oppnå ved å bruke `CultureInfo`-klassen:

```csharp
using System.Globalization;

string dateString = "12 avril 2023";
var cultureInfo = new CultureInfo("fr-FR");
DateTime parsedDate = DateTime.Parse(dateString, cultureInfo);

Console.WriteLine(parsedDate);
// Utdata: 4/12/2023 12:00:00 AM
```

**Eksakt Analysering med et Spesifikt Format:**

For scenarioer der datoer kommer i et spesifikt format som kanskje ikke er standard, kommer `DateTime.ParseExact` godt med:

```csharp
string dateString = "Wednesday, 12 April 2023";
string format = "dddd, d MMMM yyyy";
DateTime parsedDate = DateTime.ParseExact(dateString, format, CultureInfo.InvariantCulture);

Console.WriteLine(parsedDate);
// Utdata: 4/12/2023 12:00:00 AM
```

**Bruke NodaTime:**

For enda mer robust dato- og tidsanalysering, vurder å bruke det populære tredjepartsbiblioteket NodaTime. Det gir et bredt spekter av dato-/tidshåndteringsevner:

```csharp
using NodaTime;
using NodaTime.Text;

var pattern = LocalDatePattern.CreateWithInvariantCulture("yyyy-MM-dd");
var parseResult = pattern.Parse("2023-04-12");

if (parseResult.Success)
{
    LocalDate localDate = parseResult.Value;
    Console.WriteLine(localDate); // 2023-04-12
}
else
{
    Console.WriteLine("Klarte ikke å analysere.");
}
```

NodaTime tilbyr omfattende støtte for tidssoner, periode- og varighetskonsepter, og mange forskjellige kalendersystemer, noe som gjør det til et kraftig valg for kompleks dato- og tidshåndtering i .NET-applikasjoner.
