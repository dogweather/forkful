---
title:    "C#: Kalkulering av en dato i fremtiden eller fortiden"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kunne beregne datoer i fortiden eller fremtiden er en viktig ferdighet for enhver programmerer. Enten det er for å planlegge fremtidige oppgaver eller håndtere historiske data, er det ofte nødvendig å kunne utføre slike beregninger.

## Hvordan

Å beregne en dato i fortiden eller fremtiden i C# er relativt enkelt. Først må vi opprette en ny instans av DateTime-klassen, som representerer en dato og tid i C#. Deretter kan vi bruke forskjellige metoder og egenskaper for å endre og få tilgang til ulike aspekter av datoen.

La oss for eksempel beregne datoen 10 dager etter i dag:

```C#
DateTime nå = DateTime.Now; // Opprett en ny instans av DateTime med dagens dato og tid
DateTime dato = nå.AddDays(10); // Legg til 10 dager til datoen og lagre resultatet i en ny variabel

Console.WriteLine(dato); // Skriv ut resultatet: 10 dager etter i dag
```

Dette vil resultere i følgende output:

```
10/10/2021 13:30:00
```

Vi kan også beregne en dato i fortiden ved å bruke `AddDays`-metoden og et negativt tall. For eksempel:

```C#
DateTime nå = DateTime.Now; // Opprett en ny instans av DateTime med dagens dato og tid
DateTime dato = nå.AddDays(-5); // Trekke fra 5 dager fra datoen

Console.WriteLine(dato); // Skriv ut resultatet: 5 dager før i dag
```

Dette vil gi følgende output:

```
10/5/2021 13:30:00
```

Det finnes også flere andre metoder for å endre datoen, som `AddMonths(), AddYears(), AddHours(), AddMinutes()` osv. For en komplett liste over disse metodene og deres bruk, sjekk ut Microsofts offisielle dokumentasjon.

## Dypdykk

Å beregne datoer krever en god forståelse av hvordan Datetime-klassen fungerer. Det er også viktig å sørge for riktig håndtering av tidszoner og justering for skuddår. I tillegg er det ofte lurt å bruke forskjellige formateringsmetoder for å få ønsket utseende på datoen.

En annen viktig aspekt er å håndtere ulike typer datoer, som for eksempel fødselsdatoer, tidssoner og tidsstempel. Disse har alle sine egne utfordringer og krav til nøyaktighet.

## Se også

- [Microsofts offisielle dokumentasjon for DateTime-klassen](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
- [En guide til å arbeide med datoer og klokkeslett i C#](https://www.c-sharpcorner.com/UploadFile/mahesh/working-with-datetime-in-C-Sharp/) (på engelsk)
- [Eksempler på ulike bruk av DateTime-klassen i C#](https://www.c-sharpcorner.com/UploadFile/87b416/concept-of-C-Sharp-datetime/) (på engelsk)
- [Tips for å unngå vanlige feil ved bruk av DateTime-klassen](https://www.codeproject.com/Tips/653913/The-Dos-and-Don-ts-when-using-DateTime-in-Csharp) (på engelsk)