---
title:                "Finne lengden på en streng"
html_title:           "Arduino: Finne lengden på en streng"
simple_title:         "Finne lengden på en streng"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Finn Lengden på en Streng: En Innføring til C#-programmering

## Hva og Hvorfor?
Å finde lengden på en streng betyr å beregne antall tegn den inneholder. Programmerere gjør dette for å kontrollere datainngang, håndtere tekstdata, eller for å utføre spesifikke oppgaver som krever kunnskap om strengens lengde.

## Hvordan gjøre det:
Her er et eksempel på hvordan du kan finne lengden til en streng i C#:

```C#
string testStreng = "Hei, Verden!";
int lengde = testStreng.Length;
Console.WriteLine(lengde);
```
I dette eksemplet ville utskriften til konsollen være `13`. Det er hele 13 tegn - inkludert mellomrom og tegnsetting - i strengen "Hei, Verden!".

## Dypdykk:
Historisk sett har funksjonen for å finne en strenglengde vært en del av de fleste programmeringsspråk, og C# er intet unntak. Hver karakter i en streng opptar en plass i minnet, og `.Length`-egenskapen i C# returnerer antallet plasser som brukes.

Alternativt, kan du bruke `StringInfo.LengthInTextElements(streng)` fra `System.Globalization`-biblioteket for å telle antallet Unicode-tekstelementer, som kan være forskjellig fra antall tegn.

Under panseret, er `.Length`-egenskapen effektiv fordi den lagrer lengden på strengen når den blir lagd. Det betyr at å tilkalle `.Length`-egenskapen er bare å hente en allerede lagd verdi, ikke å regne den ut hver gang.

## Se også:
For videre lesning, sjekk ut de følgende kildene:
- Offisiell Microsoft-dokumentasjon for `String.Length`: [Link](https://docs.microsoft.com/en-us/dotnet/api/system.string.length)
- Diskusjon på Stack Overflow om forskjellene mellom `Length` og `LengthInTextElements`:  [Link](https://stackoverflow.com/questions/4483886/how-does-string-length-property-work-in-c-sharp)