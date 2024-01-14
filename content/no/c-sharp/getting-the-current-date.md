---
title:                "C#: Hente gjeldende dato"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor
Mange programmerere vil på et eller annet tidspunkt ha behov for å hente ut den nåværende datoen. Dette kan være for å logge når noe ble opprettet eller for å vise datoen til brukeren. Uansett årsak, er det viktig å vite hvordan du kan få tak i riktig dato og hvordan du kan formatere den slik at den passer ditt behov.

## Hvordan du gjør det
Å få tak i den nåværende datoen i C# er en enkel prosess. Først må du importere System namespace for å få tilgang til DateTime-klassen. Deretter kan du bruke metoden DateTime.Now for å få tak i dagens dato og klokkeslett. Her er et eksempel på hvordan du kan skrive dette i C#:

```C#
using System;

DateTime currentDate = DateTime.Now;
Console.WriteLine(currentDate);
```

Dette vil gi deg en output som ser slik ut:

```C#
10/02/2021 08:45:00
```

Ønsker du å bare få tak i datoen, kan du bruke metoden ToShortDateString() i stedet for å få følgende output:

```C#
10/02/2021
```

Du kan også formatere datoen på ulike måter ved hjelp av metoden ToString() og å bruke et formatmønster. Her er et eksempel hvor vi formaterer datoen til å vise bare måneden i bokstaver:

```C#
DateTime currentDate = DateTime.Now;
string formattedDate = currentDate.ToString("MMMM");
Console.WriteLine(formattedDate);
```

Dette vil gi deg følgende output:

```C#
Oktober
```

For en full liste over formatmønstre, kan du se Dokumentasjonen til Microsoft om DateTime-format.

## Dypdykk
Det er viktig å merke seg at DateTime.Now vil hente ut datoen basert på den lokale datoen på maskinen din. Hvis du ønsker å hente ut den nåværende datoen i en annen tidsone, kan du bruke metoden DateTime.UtcNow i stedet. Du kan også bruke metoden DateTime.Today for å få tak i dagens dato uten klokkeslett.

En annen nyttig funksjon i DateTime-klassen er metoden TryParseExact. Denne metoden lar deg konvertere en streng til en DateTime-objekt ved å spesifisere et formatmønster. Dette kan være nyttig hvis du henter et datostring fra en annen kilde og ønsker å konvertere den til en DateTime for å kunne arbeide med den videre i koden din.

## Se også
- [Dokumentasjon for DateTime-format fra Microsoft](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [DateTime-klassen fra Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
- [C# DateTime i praksis fra Codeburst](https://codeburst.io/c-datetime-in-practice-df642cae0487)