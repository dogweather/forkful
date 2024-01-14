---
title:                "C#: Søke og erstatte tekst"
simple_title:         "Søke og erstatte tekst"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Søking og erstatting av tekst er en viktig del av programmering, enten du er en erfaren utvikler eller en nybegynner. Det kan hjelpe deg med å effektivisere koden din og gjøre endringer i store tekstfiler på en enkel og rask måte.

## Hvordan du gjør det

For å søke og erstatte tekst i en C#-kode, kan du bruke "Replace" funksjonen. Her er et eksempel som viser hvordan du kan erstatte all tekst som inneholder "hund" med "katt" innenfor en strengvariabel:

```C#
string tekst = "Jeg liker ikke hunder, men kanskje katter er bedre.";
string nyTekst = tekst.Replace("hund", "katt");
Console.WriteLine(nyTekst);
// Output: Jeg liker ikke katter, men kanskje katter er bedre.
```

Som du kan se, erstatter "Replace" funksjonen alle forekomster av "hund" med "katt" i den valgte strengen.

Du kan også bruke "Regex" klasse for å utføre mer avanserte søk og erstatninger. Her er et eksempel som viser hvordan du kan erstatte alle tall i en streng med "X":

```C#
string nummer = "1234, ABCD";
string regex = "[0-9]";
string nyTekst = Regex.Replace(nummer, regex, "X");
Console.WriteLine(nyTekst);
// Output: XXXX, ABCD
```

Som du kan se, bruker vi et regulært uttrykk for å finne og erstatte alle tall med "X".
Det finnes mange ulike regulære uttrykk som du kan bruke for å søke og erstatte tekst, så det kan være lurt å utforske litt og se hva som fungerer best for ditt spesifikke behov.

## Dypere dykning

Når du bruker "Replace" eller "Regex" funksjoner, kan du også legge til flere parametere for å gjøre søk og erstatning enda mer tilpasset. For eksempel kan du spesifisere at søket skal ignorere store og små bokstaver, eller begrense søket til en bestemt del av teksten.

Det er også viktig å huske at søking og erstatting kan påvirke ytelsen til programmet ditt, spesielt hvis du utfører det i store tekstfiler. Derfor bør du alltid tenke på effektiviteten når du bruker disse funksjonene.

## Se også

- [C# String Replace](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=net-5.0)
- [C# Regex Class](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex?view=net-5.0) 
- [Regular Expressions Cheat Sheet](https://cheatography.com/davechild/cheat-sheets/regular-expressions/)