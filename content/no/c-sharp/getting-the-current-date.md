---
title:                "Å få gjeldende dato"
html_title:           "C#: Å få gjeldende dato"
simple_title:         "Å få gjeldende dato"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvorfor skulle noen være interessert i å få den nåværende datoen?

Det er mange grunner til at noen ville ønske å få den nåværende datoen i sine programmer. Det kan være for å vise datoen til brukeren, for å registrere når noe ble gjort eller for å gjøre beregninger basert på datoen. Med det å vite hvordan man kan få den nåværende datoen, vil du kunne forbedre funksjonaliteten og nøyaktigheten til programmene dine.

## Slik gjør du det
Du kan få den nåværende datoen i C# ved å bruke klassen DateTime. Du kan opprette et nytt DateTime-objekt og bruke metoden Now() for å få den nåværende datoen og klokkeslettet. La oss se på et eksempel:

```C#
DateTime nåværendeDato = DateTime.Now;
Console.WriteLine("Den nåværende datoen er: " + nåværendeDato);
```
Dette vil gi følgende output:

```
Den nåværende datoen er: 09.06.2021 13:24:18
```
Du kan også bruke metoder som ToString() for å formatere utskriften på en spesifikk måte. For eksempel, hvis du bare ønsker å få ut datoen uten klokkeslettet, kan du bruke formatet "dd.MM.yyyy":

```C#
DateTime nåværendeDato = DateTime.Now;
Console.WriteLine("Den nåværende datoen er: " + nåværendeDato.ToString("dd.MM.yyyy"));
```
Dette vil gi følgende output:

```
Den nåværende datoen er: 09.06.2021
```

## Dypdykk
DateTime-klassen inneholder også andre nyttige metoder for å utføre operasjoner på datoen, som for eksempel å legge til eller trekke fra dager, timer eller minutter. Du kan også bruke DateTime.TryParse() for å prøve å konvertere en streng til en DateTime-verdi.

Det er viktig å merke seg at den nåværende datoen som returneres av metoden Now(), er basert på datoen og klokkeslettet på datamaskinen din. Derfor vil den være forskjellig for forskjellige brukere eller på forskjellige datamaskiner. Hvis du ønsker å få den nåværende datoen basert på en spesifikk tidssone, kan du bruke klassen TimeZone og dens metoder.

## Se også
Her er noen nyttige ressurser for å lære mer om å få den nåværende datoen i C#:

- [DateTime-klasse (C# programmeringsguide)](https://docs.microsoft.com/nb-no/dotnet/api/system.datetime?view=net-5.0)
- [DateTime.Now Metode (System) (C# programmeringsguide)](https://docs.microsoft.com/nb-no/dotnet/api/system.datetime.now?view=net-5.0)
- [DateTime.TryParse Metode (System) (C# programmeringsguide)](https://docs.microsoft.com/nb-no/dotnet/api/system.datetime.tryparse?view=net-5.0)
- [TimeZone-klasse (C# programmeringsguide)](https://docs.microsoft.com/nb-no/dotnet/api/system.timezone?view=net-5.0)