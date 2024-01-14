---
title:    "C#: Konvertering av streng til små bokstaver"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å konvertere en streng til små bokstaver er en vanlig oppgave i programmering, spesielt når man jobber med tekstbehandling. Dette kan være nyttig for å sammenligne strenger eller lage mer leselige utskrifter. Å kunne gjøre dette effektivt er derfor viktig for enhver utvikler.

## Hvordan

Å konvertere en streng til små bokstaver er veldig enkelt i C#. Det finnes flere metoder som kan brukes, men her er en metode som er spesielt effektiv og enkel å implementere:

```C#
string text = "HEI, Dette ER EN TEKST";

// Konverterer til små bokstaver ved hjelp av ToLower() metoden
string converted = text.ToLower();

Console.WriteLine(converted);
```
Output: "hei, dette er en tekst"

Denne metoden bruker ToLower() metoden for å konvertere alle bokstaver i teksten til små bokstaver. Dette gjør det mulig å sammenligne strenger selv om de har forskjellige former av bokstaver. For eksempel vil "hei" og "HEI" være like etter konverteringen.

## Dypdykk

Å konvertere en streng til små bokstaver virker kanskje enkelt, men det er noen viktige ting å huske på. For det første vil denne metoden bare konvertere standard engelske bokstaver til små bokstaver. Dette betyr at bokstaver med aksenter eller andre spesielle tegn vil ikke bli konvertert riktig.

I tillegg er det viktig å være oppmerksom på at konverteringen vil bare endre tegn som er en del av alfabetet. Dette betyr at tall, spesielle tegn og mellomrom ikke vil bli endret. Derfor er det viktig å være forsiktig når man bruker denne metoden og å kontrollere alle tegn i strengen man ønsker å konvertere.

## Se Også

- [C# String.ToLower Method (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower)
- [C# String Comparison and Culture (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/standard/base-types/string-comparison-and-culture)