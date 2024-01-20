---
title:                "Konvertere en streng til små bokstaver"
html_title:           "Arduino: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Konvertering av streng til små bokstaver i C#

## Hva & Hvorfor?
Å konvertere en streng til små bokstaver er prosessen med å endre alle store bokstaver i en tekst til deres tilsvarende småbokstaver. Programmører gjør dette for å normalisere og forenkle tekstsammenligninger og søk.

## Hvordan å:
Her er et eksempel på C# -kode som konverterer en streng til små bokstaver:

```C#
string minTekst = "Hei Verden!";
string minTekstILavCase = minTekst.ToLower();
Console.WriteLine(minTekstILavCase);
```

Kjører du dette, vil output være:
`hei verden!`

## Dypdykk
Denne metoden for å konvertere strenger til små bokstaver har blitt brukt siden de tidlige dagene av programmering, og er en nøkkelfunksjon i mange programmeringsspråk, ikke bare C#. 

Det finnes alternativer til `.ToLower()`, som for eksempel `.ToLowerInvariant()`. Sistnevnte vil konvertere strengen til små bokstaver uavhengig av kulturelle innstillinger på systemet, mens `.ToLower()` kan gi forskjellige resultater basert på systemets lokalisering.

C# utfører denne konverteringen ved hjelp av Unicode data, noe som betyr at den takler de fleste språk og tegnsett.

## Se også:
For mer informasjon om å jobbe med strenger i C#, se disse kildene:

- [Microsofts Dokumentasjon av String.ToLower()](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=net-5.0)
- [Microsofts Dokumentasjon av String.ToLowerInvariant()](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolowerinvariant?view=net-5.0)
- [Stack Overflow: ToLower vs ToLowerInvariant](https://stackoverflow.com/questions/2801508/lowercase-invariant-in-c-sharp-and-its-purpose)