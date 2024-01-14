---
title:    "C#: Søke og erstatte tekst"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Hvorfor

Når du jobber med programmering, er det ofte nødvendig å søke gjennom tekst og erstatte deler av den med annen tekst. Dette kan være en tidkrevende oppgave, spesielt når det er mange forekomster som må endres. Ved å bruke de riktige metodene og teknikkene, kan du gjøre denne oppgaven raskt og effektivt.

## Hvordan gjøre det

Det er flere måter å søke gjennom tekst og erstatte deler av den på i C#. En vanlig måte er å bruke metoden `Replace` fra `System.String`-klassen. Denne metoden tar inn to argumenter, en streng som skal erstattes og en streng som skal erstatte den.

```C#
string tekst = "Dette er en tekst som skal endres.";
string nyTekst = tekst.Replace("endres", "erstattes");
Console.WriteLine(nyTekst);
```

Dette vil gi følgende utskrift:

```
Dette er en tekst som skal erstattes.
```

Du kan også bruke `Replace`-metoden til å erstatte deler av en streng basert på et mønster. For dette trenger du å bruke `Regex`-klassen fra `System.Text.RegularExpressions`-namespacet. Her er et eksempel på hvordan du kan bruke det:

```C#
string tekst = "Dette er en tekst med tall 123 og symboler @$!";
string nyTekst = Regex.Replace(tekst, "[0-9@$!]", "");
Console.WriteLine(nyTekst);
```

Dette vil gi følgende utskrift:

```
Dette er en tekst med tall og symboler.
```

Det finnes også andre måter å søke og erstatte tekst på i C#, avhengig av dine spesifikke behov og preferanser. Ved å utforske dokumentasjonen og eksperimentere med metodene, kan du finne den beste måten å håndtere dette på for din kode.

## Dypdykk

`Replace`-metoden er bare en av mange metoder som kan brukes til å søke og erstatte tekst i C#. Andre nyttige metoder inkluderer `IndexOf` og `LastIndexOf` fra `System.String`-klassen, samt `ReplaceAll` fra `Regex`-klassen.

I tillegg kan du bruke ulike regex-mønstre for mer avansert søk og erstatning. Du kan for eksempel bruke grupper og tilbakereferanser til å erstatte tekst basert på et mønster som matcher deler av teksten.

Søking og erstatning av tekst kan også kombineres med andre funksjonaliteter i C#, som filhåndtering og loop-konstruksjoner, for å automatisere og forenkle prosessen enda mer.

## Se også

- [Microsoft Docs: String.Replace method in C#](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=net-5.0)
- [Microsoft Docs: Regex.Replace method in C# ](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex.replace?view=net-5.0)
- [C# Regex Cheat Sheet](https://www.thinkprogramming.co.uk/regex-cheat-sheet/)