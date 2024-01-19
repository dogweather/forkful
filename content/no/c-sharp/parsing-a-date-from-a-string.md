---
title:                "Tolke en dato fra en streng"
html_title:           "Bash: Tolke en dato fra en streng"
simple_title:         "Tolke en dato fra en streng"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å parse en dato fra en streng betyr å omdanne strengen til en datatypenhet. Dette gjøres for å manipulere, analysere, beregne eller sende data på en mer systematisk måte.

## Hvordan:

Å parse en dato involverer bruken av DateTime.TryParse eller DateTime.Parse funksjon i C#.
Her er et eksempel:
```C#
string strengDato = "24.12.2021";
DateTime parsedDato;

bool succefullParse = DateTime.TryParse(strengDato, out parsedDato);

if (succefullParse)
    Console.WriteLine($"Parsed dato: {parsedDato.ToShortDateString()}");
else
    Console.WriteLine("Ugyldig dato streng!");
```
Utdata for ovennevnte kode vil være:
```
Parsed dato: 24.12.2021
```
## Dyp Dykk:
1. Historisk Kontekst: Parsing funksjon eksisterer siden tidlige versjoner av C#, og det har blitt raffinert gjennom hver utgave, gjør operasjonen enklere og mer presis.
2. Alternativer: Du kan også bruke DateTime.ParseExact hvis du kjenner det eksakte formatet på datostrengen.
3. Implementasjondetaljer: Parsing kaster unntak ved ugyldige verdier. Så husk å behandle disse situasjonene ved å bruke TryParse metoden som unngår unntak ved å returnere en boolsk verdi som indikerer suksess eller feil.

## Se Også:
1. [Microsoft Dokumentasjon på DateTime.Parse](https://docs.microsoft.com/nb-no/dotnet/api/system.datetime.parse?view=netframework-4.7.2)
2. [Microsoft Dokumentasjon på DateTime.TryParse](https://docs.microsoft.com/nb-no/dotnet/api/system.datetime.tryparse?view=netframework-4.7.2)
3. [Microsoft Dokumentasjon på DateTime.ParseExact](https://docs.microsoft.com/nb-no/dotnet/api/system.datetime.parseexact?view=net-5.0)