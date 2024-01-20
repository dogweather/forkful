---
title:                "Sammenligner to datoer"
html_title:           "Clojure: Sammenligner to datoer"
simple_title:         "Sammenligner to datoer"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?

Å sammenligne to datoer i programmering betyr å bestemme hvilken som er tidligere, senere eller meget mulig at de er de samme. Vi gjør dette i scenarioer hvor vi vilgradere data etter tid, eller når vi vil kontrollere tidsdifferansen mellom to datoer.

## Hvordan:

For å sammenligne datohendelser i C#, så bruker vi normalt DateTime.Compare() metoden. 

```C#
DateTime date1 = new DateTime(2021, 11, 1);
DateTime date2 = new DateTime(2022, 11, 1);
 
int result = DateTime.Compare(date1, date2);
               
if (result < 0)
  Console.WriteLine("date1 er tidligere enn date2.");
else if (result == 0)
  Console.WriteLine("date1 og date2 er det samme.");
else 
  Console.WriteLine("date1 er senere enn date2.");
```

Koden over vil output:

```C#
date1 er tidligere enn date2.
```

## Dyp Dykk

Historisk sett har sammenligning av datoer i C# blitt håndtert med DateTime.Compare()-metoden - som sammenligner to forekomster av DateTime og returnerer en heltall som indikerer om den første forekomsten er tidligere, den samme, eller senere enn den andre forekomsten. 

En alternativ måte er å bruke CompareTo()-metoden:

```C#
int result = date1.CompareTo(date2);
```

Eller rett og slett sammenligne dem direkte:

```C#
if (date1 > date2)
{
  Console.WriteLine("date1 er senere enn date2.");
}
else if (date1 < date2)
{
  Console.WriteLine("date1 er tidligere enn date2.");
}
else
{
  Console.WriteLine("date1 og date2 er det samme.");
}
```

Når det kommer til implementeringsdetaljer, så blir datoer lagret som et 64-bits heltall som representerer antall ticks siden 1. januar 0001 kl 00:00:00 UTC.

## Se Også 

For en mer detaljert oversikt over hvordan du jobber med datoer og klokkeslett i C#, sjekk ut Microsofts offisielle dokumentasjon: [DateTime-strukturen i .NET](https://docs.microsoft.com/nb-no/dotnet/api/system.datetime) og [Working with dates and times in .NET](https://docs.microsoft.com/nb-no/dotnet/standard/datetime/).