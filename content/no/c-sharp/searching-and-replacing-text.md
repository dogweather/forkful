---
title:                "Søke og erstatte tekst"
html_title:           "C#: Søke og erstatte tekst"
simple_title:         "Søke og erstatte tekst"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor
Ofte i programmering må man bytte ut tekst i et dokument eller en fil, enten det er for å rette feil eller oppdatere informasjon. Dette kan være en tidkrevende oppgave hvis man må gjøre det manuelt, derfor er det viktig å kunne bruke et programmeringsspråk som C# for å automatisere denne prosessen.

## Slik gjør du det
For å søke og erstatte tekst i C#, må man bruke "Replace" metoden. Her er et eksempel på hvordan man bruker den:

```
string originalText = "Hei, verden!";
string newText = originalText.Replace("Hei", "Hallo");
Console.WriteLine(newText);
```
Dette vil resultere i at teksten "Hallo, verden!" blir skrevet ut i konsollen. Her har vi erstattet "Hei" med "Hallo" i den originale teksten.

For å søke og erstatte i en fil, må man først lese inn filen og lagre den i en string. Deretter kan man bruke "Replace" metoden på denne stringen og til slutt skrive den endrede teksten tilbake til filen.

```
string fileText = File.ReadAllText("tekstfil.txt");
string newText = fileText.Replace("gammel tekst", "ny tekst");
File.WriteAllText("tekstfil.txt", newText);
```

## Dykk dypere
Det finnes flere alternativer når man arbeider med "Replace" metoden i C#. Man kan for eksempel spesifisere om teksten man søker etter skal være case-sensitive eller ikke ved å bruke "StringComparison" parameteren. Man kan også angi om man vil begynne å søke etter teksten fra en bestemt posisjon i stringen ved å bruke "startsWith" og "indexOf" metoden. Ved å studere dokumentasjonen til "Replace" metoden, kan man lære mer om alle mulighetene og finne ut hvilke som passer best for ens eget prosjekt.

## Se også
- [C# dokumentasjon for Replace metoden](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=netcore-3.1)
- [Guide til å arbeide med tekst i C#](https://www.tutorialspoint.com/csharp/csharp_strings.htm)