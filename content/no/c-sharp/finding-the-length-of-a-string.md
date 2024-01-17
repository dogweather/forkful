---
title:                "Å finne lengden av en streng"
html_title:           "C#: Å finne lengden av en streng"
simple_title:         "Å finne lengden av en streng"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Hvorfor og hvordan finne lengden til en streng i C#

## Hva og hvorfor?

Å finne lengden til en streng betyr rett og slett å finne antall bokstaver eller tegn i en tekststreng. Dette kan være nyttig for programmerere når de trenger å håndtere ulike typer data eller utføre spesifikke oppgaver, for eksempel å validere input fra brukere eller beregne størrelsen på en tekstfil.

## Hvordan gjøre det:

For å finne lengden til en streng i C# kan du bruke den innebygde ```Length``` -funksjonen. Denne funksjonen tar inn en streng som parameter og returnerer et heltall som tilsvarer lengden på strengen.

```C#
string tekst = "Hei, dette er en tekststreng.";
int lengde = tekst.Length; // lengden vil være 29
```

Du kan også bruke ```Count``` -funksjonen, som fungerer på samme måte.

```C#
string tekst = "Halla!";
int lengde = tekst.Count(); // lengden vil være 6
```

## Dykk dypere:

Historisk sett har beregning av strenglengde vært en viktig del av programmering, da det har vært nødvendig for å håndtere ulike typer data. I tillegg til de innebygde funksjonene som er nevnt ovenfor, finnes det også alternative metoder for å finne lengden til en streng ved å bruke løkker eller egendefinerte funksjoner.

Når det gjelder implementering, bruker C# en forhåndsinnstilt ```System.String``` -klasse som har de nødvendige funksjonene for å finne lengden til en streng. Denne klassen er også ansvarlig for å håndtere andre aspekter ved strenger, som å sammenligne og redigere dem.

## Se også:

For mer informasjon om hvordan du finner lengden til en streng i C#, kan du sjekke ut følgende lenker:

- [Microsoft's dokumentasjon om C# strenger](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/)
- [En guide for å jobbe med strenger i C#](https://www.tutorialsteacher.com/csharp/csharp-string)
- [Bruk av LINQ-funksjoner for å finne lengden til en streng](https://www.c-sharpcorner.com/blogs/how-to-find-length-of-a-string-in-linq)