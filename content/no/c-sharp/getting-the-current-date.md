---
title:                "Få den gjeldende datoen"
html_title:           "Haskell: Få den gjeldende datoen"
simple_title:         "Få den gjeldende datoen"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Få Dagens Dato: En enkel guide til C# programmering

## Hva & Hvorfor?
Få dagens dato i programmering er å hente nåværende dato og tid fra systemet. Dette er nyttig for å logge hendelser, tidtakingsfunksjoner, og for å gi kontekstuell informasjon til brukere.

## Hvordan:

Følgende eksempel viser hvordan du kan få den nåværende datoen i C#:

```C#
using System;

class Program
{
    static void Main()
    {
        DateTime currentDate = DateTime.Now;
        Console.WriteLine(currentDate);
    }
}
```

Når du kjører dette programmet, vil det vise en output som ligner på dette:

```C#
2022-02-23 13:56:02
```

## Deep Dive:

Historie: I tidligere versjoner av .NET og C#, brukte utviklere `DateTime.Now` for å få den nåværende datoen og tiden. Men, i .NET Core og senere versjoner av C#, anbefales det å bruke `DateTimeOffset.Now` fordi det også tar hensyn til tidssone offset.

Alternativer: I tillegg til `DateTime.Now`, kan du også bruke `DateTime.UtcNow` for å få UTC tid, eller `DateTime.Today` for å få dagens dato uten tid.

Implementasjon: `DateTime.Now` får dato og tid fra operativsystemets systemtid. Det er en egenskap av `DateTime` strukturen, og returnerer et objekt av samme type.

## Se Også:

- [Microsofts Dokumentasjon på DateTime.Now](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.now?view=net-6.0)
- [StackOverflow Diskusjon om DateTime vs DateTimeOffset](https://stackoverflow.com/questions/4331189/datetime-vs-datetimeoffset)
- [DotNet Perls Guide på DateTime](https://www.dotnetperls.com/datetime)