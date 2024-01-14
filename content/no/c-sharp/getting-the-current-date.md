---
title:                "C#: Hente nåværende dato"
simple_title:         "Hente nåværende dato"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor
Ved å få den nåværende datoen i et C# program, kan du vise brukeren nøyaktig tid og dato, eller bruke det til å planlegge fremtidige hendelser.

## Hvordan 
Det er flere måter å få den nåværende datoen i C#. En enkel måte er å bruke DateTime.Now funksjonen. Her er et eksempel på hvordan du kan implementere dette:

```C#
DateTime now = DateTime.Now;
Console.WriteLine("Dagens dato er: " + now);
```

Denne koden vil gi deg følgende utgang:

```
Dagens dato er: 02.10.2021 12:30:00
```

Du kan også formatere datoen på forskjellige måter ved å bruke DateToString-funksjonen. Her er et eksempel på hvordan det kan se ut:

```C#
DateTime now = DateTime.Now;
string formattedDate = now.ToString("dd/MM/yyyy");
Console.WriteLine("Dagens dato er: " + formattedDate);
```

Dette vil gi deg følgende utgang:

```
Dagens dato er: 02/10/2021
```

## Deep Dive
DateTime structen i C# gir mange nyttige metoder og egenskaper for å få den nåværende datoen. For eksempel kan du bruke DateTime.Today for å få ut bare dagens dato uten tid. Du kan også bruke DateTime.Parse for å konvertere en streng til en DateTime-objekt.

En annen viktig ting å huske på er at DateTime er avhengig av systemets klokkeslett. Hvis systemklokken endres, vil også den nåværende datoen endres. Det er derfor viktig å oppdatere systemklokken regelmessig for å få nøyaktige resultater.

## Se også
- [DateTime Struct Dokumentasjon] (https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
- [DateTime metoder og egenskaper] (https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0#methods)
- [DateTime.Now Dokumentasjon] (https://docs.microsoft.com/en-us/dotnet/api/system.datetime.now?view=net-5.0)
- [DateTime.ToString Metode Dokumentasjon] (https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring?view=net-5.0)