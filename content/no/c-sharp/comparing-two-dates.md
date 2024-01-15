---
title:                "Sammenligning av to datoer"
html_title:           "C#: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor
I noen programmeringsspråk kan vi sammenligne to datoer ved å bruke vanlige logiske operatører som "> (større enn)" eller "< (mindre enn)". Men i C# må vi bruke en annen tilnærming på grunn av dets DateTime struktur. I denne artikkelen vil jeg vise deg hvordan du effektivt kan sammenligne to datoer i C#.

## Hvordan
For å sammenligne to datoer i C#, må vi først konvertere begge datoene til DateTime objekter. Deretter kan vi bruke DateTime sin Compare metode for å få en numerisk verdi tilbake, som kan tolkes på følgende måte:

- Hvis resultatet er mindre enn 0, betyr det at den første datoen er tidligere enn den andre datoen.
- Hvis resultatet er 0, betyr det at begge datoene er like.
- Hvis resultatet er større enn 0, betyr det at den første datoen er senere enn den andre datoen.

La oss ta en titt på et eksempel:

```C#
DateTime date1 = new DateTime(2020, 1, 1);
DateTime date2 = new DateTime(2020, 2, 1);
int result = DateTime.Compare(date1, date2);
Console.WriteLine(result); // resultatet vil bli -1
```

Som du kan se, blir resultatet -1 siden januar kommer før februar. Vi kan også bruke logiske operatører som ">=" og "<=" til å sammenligne datoer ved hjelp av denne numeriske verdien. Her er et annet eksempel:

```C#
DateTime date1 = new DateTime(2020, 1, 1);
DateTime date2 = new DateTime(2020, 2, 1);
if (DateTime.Compare(date1, date2) <= 0)
{
    Console.WriteLine("date1 er lik eller kommer før date2");
}
```

Dette vil skrive ut "date1 er lik eller kommer før date2" siden resultatet er mindre enn eller lik 0.

## Deep Dive
DateTime objektet i C# representerer en bestemt dato og klokkeslett. I tillegg til å sammenligne datoer, kan vi også bruke DateTime til å utføre en rekke operasjoner på datoer, som å legge til eller trekke fra et bestemt antall dager, måneder eller år. Det finnes også flere nyttige metoder, for eksempel AddDays(), AddMonths() og AddYears(), som lar deg justere en dato etter behov. Ønsker du å lese mer om DateTime, kan du sjekke ut dokumentasjonen her: [https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=netcore-3.1](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=netcore-3.1)

## Se også
- [Slik bruker du DateTime i C#](https://www.w3schools.com/cs/cs_datetime.asp)
- [Sammenligne to datoer i C#](https://www.tutorialspoint.com/csharp/csharp_date_time.htm)
- [DateTime klasse i C#](https://www.geeksforgeeks.org/datetime-in-c-sharp/)