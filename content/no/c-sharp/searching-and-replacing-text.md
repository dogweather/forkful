---
title:                "Søking og erstatning av tekst"
html_title:           "Lua: Søking og erstatning av tekst"
simple_title:         "Søking og erstatning av tekst"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Søke og erstatte tekst i C#: En grunnleggende guide

## Hva & Hvorfor?
Å søke og erstatte tekst er prosessen med å finne bestemte strenger i en tekstmasse og erstatte dem med noe annet. Programmet utfører dette for å manipulere og behandle data mer effektivt.

## Hvordan:
Her er noen enkle eksempler på hvordan søke og erstatte tekst i C#.

```C#
string tekst = "Hei, jeg elsker å koding!";
string nyTekst = tekst.Replace("elsker", "hater");

Console.WriteLine(nyTekst);
```
Utdata: "Hei, jeg hater å koding!"

```C#
string tekst = "Hei, jeg elsker å koding! Jeg elsker det virkelig!";
string nyTekst = tekst.Replace("elsker", "hater");

Console.WriteLine(nyTekst);
```
Utdata: "Hei, jeg hater å koding! Jeg hater det virkelig!"

## Dypdykk
C# er et objektorientert programmeringsspråk utviklet i 2002 av Microsoft. Tekstsøk og erstatning er en langvarig funksjon i programmering og er en vanlig oppgave i tekstbehandling.

Alternativer til 'Replace'-metoden kan være bruk av regulære uttrykk ('Regex') for mer komplekse søke- og erstatningsoperasjoner, men 'Replace'-metoden er generelt sett mer rett frem og brukervennlig.

Implementeringsdetaljer: 'Replace'-funksjonen i C# opererer ved å først finne indeksposisjonen til søkestrengen i tekststrengen. Deretter erstatte den denne delen av teksten med erstatningsteksten.

## Se Også
Du kan finne mer detaljerte informasjon og eksempler på disse nettstedene:
- [Microsoft C# Guide](https://docs.microsoft.com/nb-no/dotnet/csharp/)
- [C# Text Manipulation](https://www.tutorialsteacher.com/csharp/csharp-string-replace)
- [Regex i C#](https://docs.microsoft.com/nb-no/dotnet/standard/base-types/regular-expression-language-quick-reference)