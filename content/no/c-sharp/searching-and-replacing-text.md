---
title:    "C#: Søking og erstatning av tekst"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

#### Hvorfor

Å søke og erstatte tekst er en vanlig oppgave for de som jobber med programmering. Det kan være en enkel og effektiv måte å endre tekst i store mengder med kode på, og kan spare deg for mye tid og frustrasjon. Derfor er det viktig å ha gode kunnskaper om hvordan man kan gjøre dette raskt og effektivt med C#.

#### Hvordan

For å søke og erstatte tekst i C#, kan du bruke metoden "Replace" fra String-klassen. Denne metoden tar inn to parametere, tekst som skal erstattes, og ny tekst som skal plasseres. Her er et eksempel på hvordan du kan bruke Replace i en Console-applikasjon:

```C#
// Definerer en streng med tekst som vi vil endre
string tekst = "Hei, jeg heter John og jeg elsker å programmere.";

// Bruker Replace-metoden for å erstatte "John" med "Jane"
string nyTekst = tekst.Replace("John", "Jane");

// Skriver ut den nye teksten
Console.WriteLine(nyTekst);

// Output: Hei, jeg heter Jane og jeg elsker å programmere.
```

Som du kan se, er den originale teksten nå endret til "Jane" i stedet for "John".

#### Deep Dive

I tillegg til å kunne erstatte tekst, er det også mulig å bruke Replace-metoden til å fjerne tekst helt. Dette kan gjøres ved å sende inn en tom streng som erstattning. Her er et eksempel på hvordan du kan fjerne all tekst som inneholder tall fra en streng:

```C#
// Definerer en streng med tall og tekst
string tekst = "Det var en gang 123 en katt som hoppet over 456 månen.";

// Bruker Replace til å fjerne tallene
string nyTekst = tekst.Replace("123", "").Replace("456", "");

// Skriver ut den nye teksten
Console.WriteLine(nyTekst);

// Output: Det var en gang  en katt som hoppet over  månen.
```

Som du kan se, har alle tallene blitt fjernet fra teksten.

Det er også viktig å merke seg at Replace-metoden er casesensitiv, noe som betyr at store og små bokstaver vil bli behandlet forskjellig. Det er derfor lurt å dobbeltsjekke at du har riktig casing når du bruker Replace-metoden.

#### Se også

- [C# Dokumentasjon for Replace-metoden](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=netcore-3.1)
- [Tutorial: Søke og erstatte tekst i C#](https://www.tutorialspoint.com/csharp/manage_string_replace.htm)