---
title:                "C#: Sammenligne to datoer"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor
Mange programmerere trenger å sammenligne to datoer i ulike situasjoner, for eksempel for å sjekke om en bestemt hendelse har skjedd før eller etter en annen hendelse. Dette kan være nyttig for å organisere og filtrere data, generere rapporter og utføre andre oppgaver i et program.

## Hvordan gjøre det i C#
Det er mange måter å sammenligne datoer på i C#, men det mest vanlige er å bruke `DateTime`- og `TimeSpan`-klassene. Her er et eksempel på hvordan du kan sammenligne to datoer og få ut en `TimeSpan`-verdi som viser tidsforskjellen mellom dem:

```c#
DateTime dato1 = new DateTime(2020, 5, 15);
DateTime dato2 = new DateTime(2021, 1, 1);
TimeSpan forskjell = dato2 - dato1;
Console.WriteLine($"Dato2 er {forskjell.Days} dager senere enn dato1");
```

I dette tilfellet vil utskriften bli "Dato2 er 231 dager senere enn dato1". Det er også mulig å sammenligne datoer ved hjelp av metoder som `Compare`, `Equals` og `CompareTo` i `DateTime`-klassen. Her er et eksempel på hvordan du kan bruke `CompareTo` for å sammenligne to datoer og få ut en `int`-verdi som viser resultatet:

```c#
DateTime date1 = new DateTime(2020, 5, 15);
DateTime date2 = new DateTime(2021, 1, 1);
int resultat = date1.CompareTo(date2);
if (resultat == 0)
{
    Console.WriteLine("Datoene er like");
}
else if (resultat < 0)
{
    Console.WriteLine("date1 er tidligere enn date2");
}
else
{
    Console.WriteLine("date1 er senere enn date2");
}
```

Her vil utskriften være "date1 er tidligere enn date2". Du kan også bruke `Compare`- og `Equals`-metodene på `TimeSpan`-objekter for å sammenligne tidsforskjeller.

## Dypdykk
Når du sammenligner to datoer, er det viktig å være klar over at det kan være ulikheter mellom datoer og klokkeslett, og at dette kan påvirke resultatene dine. For eksempel vil ikke to `DateTime`-objekter med samme dato men ulik klokkeslett bli ansett som like ved hjelp av `Equals`-metoden. Det er derfor viktig å være nøye med hvilken metode du bruker for å sammenligne datoer, og å ta hensyn til eventuelle tidsforskjeller.

## Se også
- [DateTime-strukturen på Microsoft Docs](https://docs.microsoft.com/nb-no/dotnet/api/system.datetime?view=net-5.0)
- [TimeSpan-strukturen på Microsoft Docs](https://docs.microsoft.com/nb-no/dotnet/api/system.timespan?view=net-5.0)
- [Sammenligne tidsintervaller og datoer i C#](https://www.c-sharpcorner.com/article/comparing-time-intervals-and-dates-in-c-sharp/)