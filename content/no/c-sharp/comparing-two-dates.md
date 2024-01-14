---
title:    "C#: Sammenligning av to datoer"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Hvorfor

Å sammenligne to datoer kan være nyttig i mange programmeringsoppgaver, for eksempel i å sjekke om en bestemt dato er før eller etter en annen dato. Det kan også brukes til å beregne tidsintervaller og kontrollere om en dato faller innenfor et bestemt tidsintervall. Ved å kunne sammenligne datoer, kan du lage mer dynamiske og nøyaktige programmer.

## Hvordan

For å sammenligne to datoer i C#, kan du bruke Compare-metoden i DateTime-klasse. Her er et eksempel på hvordan du kan sammenligne to datoer og få ut en tekstbasert beskrivelse av resultatet:

```C#
DateTime førsteDato = new DateTime(2021, 10, 10);
DateTime andreDato = new DateTime(2021, 11, 15);

int resultat = DateTime.Compare(førsteDato, andreDato);

if (resultat < 0)
{
    Console.WriteLine("{0} er før {1}", førsteDato.ToShortDateString(), andreDato.ToShortDateString());
}
else if (resultat > 0)
{
    Console.WriteLine("{0} er etter {1}", førsteDato.ToShortDateString(), andreDato.ToShortDateString());
}
else
{
    Console.WriteLine("{0} og {1} er like", førsteDato.ToShortDateString(), andreDato.ToShortDateString());
}
```

Output: 10.10.2021 er før 15.11.2021.

I dette eksempelet bruker vi Compare-metoden til å sammenligne to datoer og få et resultat i form av en integer. Dersom resultatet er mindre enn 0, betyr det at første dato er før andre dato, mens et resultat større enn 0 betyr at første dato er etter andre dato. En verdi på 0 betyr at datoene er like.

## Dypdykk

Når du sammenligner datoer i C#, må du være oppmerksom på at tidspunktet også blir tatt med i beregningen. Dette betyr at om to datoer er like, men har forskjellige tidspunkter, vil Compare-metoden gi et annet resultat.

I tillegg kan du også bruke andre metoder i DateTime-klassen til å sammenligne datoer, som for eksempel Equals-metoden eller større/lik og mindre/lik operatører. Det er også viktig å merke seg at DateTime-objekter er immutable, det vil si at de ikke kan endres. Derfor må du lage et nytt DateTime-objekt dersom du ønsker å endre datoen du sammenligner med.

## Se Også

- [DateTime.Compare Metode (System) (msdn.microsoft.com)](https://docs.microsoft.com/nb-no/dotnet/api/system.datetime.compare?view=net-5.0)
- [DateTime.Equals Metode (System) (msdn.microsoft.com)](https://docs.microsoft.com/nb-no/dotnet/api/system.datetime.equals?view=net-5.0)
- [How to: Extract the Day of the Week from a Date (c-sharpcorner.com)](https://www.c-sharpcorner.com/article/c-sharp-how-to-extract-the-day-of-the-week-from-a-datetime-in-net/)