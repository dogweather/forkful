---
title:                "Omdanner en dato til en streng"
html_title:           "C#: Omdanner en dato til en streng"
simple_title:         "Omdanner en dato til en streng"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Konvertering av en dato til en streng betyr å endre formatet på en dato-verdi til tekst som kan vises for brukeren. Programmere må gjøre dette for å vise datoer på en mer leselig og brukervennlig måte.

## Hvordan:
En enkel måte å konvertere en dato til en streng er å bruke metoden ToString(). Her er et eksempel:

```C#
DateTime now = DateTime.Now;
string dateAsString = now.ToString("dd/MM/yyyy");
Console.WriteLine(dateAsString);
```

Output: 16/10/2021

En annen måte å konvertere en dato til en streng på er å bruke metoden ToShortDateString(). Dette vil gi en kortere versjon av datoen, uten klokkeslett. Her er et eksempel:

```C#
DateTime now = DateTime.Now;
string dateAsString = now.ToShortDateString();
Console.WriteLine(dateAsString);
```

Output: 16/10/2021

## Dykk dypere:
Konvertering av en dato til en streng kan være nyttig når man vil vise datoen på en spesifikk måte, som for eksempel i en rapport eller på en nettside. Det kan også bli brukt til å sammenligne datoer eller ordne dem i en bestemt rekkefølge. Alternativt, i stedet for å bruke ToString() eller ToShortDateString(), kan man også bruke metoden ToString("format") som tar inn et formatargument som gir mer kontroll over hvordan datoen blir vist. For eksempel, ved å bruke ToString("yyyy-MM-dd") vil datoen bli vist i formatet "2021-10-16". Dette kan være nyttig når man trenger å sortere datoer etter år, måned og dag.

## Se også:
- [DateTime.ToString() metode dokumentasjon](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring?view=net-5.0)
- [DateTime.ToShortDateString() metode dokumentasjon](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.toshortdatestring?view=net-5.0)