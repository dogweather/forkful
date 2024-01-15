---
title:                "Skriving til standardfeil"
html_title:           "C#: Skriving til standardfeil"
simple_title:         "Skriving til standardfeil"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å skrive til standard error er en viktig del av programmørens verktøykasse. Det lar oss uttrykke feil og advarsler på en mer effektiv måte, og hjelper oss med å feilsøke og fange uventede feil i koden vår.

## Hvordan

For å skrive til standard error i C#, kan vi bruke Console.Error.WriteLine() -funksjonen. Denne funksjonen tar inn en string som parameter og skriver ut den gitte teksten til standard error. La oss se på et eksempel:

```
Console.Error.WriteLine("Dette er en feilmelding.");
```
Output:
```
Dette er en feilmelding.
```

Vi kan også bruke Console.Error.Write() -funksjonen hvis vi ikke ønsker å legge til en linjeskift på slutten av teksten. Her er et eksempel på hvordan dette ser ut i koden:

```
Console.Error.Write("Dette er en advarsel: ");
Console.Error.WriteLine("Vær forsiktig!");
```
Output:
```
Dette er en advarsel: Vær forsiktig!
```

## Deep Dive

Det å skrive til standard error lar oss adskille utskrifter som er ment for feilhåndtering fra vanlige utskrifter på standard output. Det gir også en bedre organisering av koden vår og gjør det lettere å finne og håndtere feil.

Det er viktig å merke seg at standard error er forskjellig fra standard output, og de to bør ikke forveksles. Standard error blir ofte brukt til å skrive ut feilmeldinger og advarsler, mens standard output brukes til å skrive ut ønsket utdata fra programmet.

## Se Også

- [Console.Error.WriteLine Metode (System)](https://docs.microsoft.com/nb-no/dotnet/api/system.console.error.writeline?view=netframework-4.8)
- [Console.Error.Write Metode (System)](https://docs.microsoft.com/nb-no/dotnet/api/system.console.error.write?view=netframework-4.8)
- [Hvordan skrive ut til standard output i C#](https://www.itavisen.no/2020/04/19/c-sharp-skrive-till-standard-output/)