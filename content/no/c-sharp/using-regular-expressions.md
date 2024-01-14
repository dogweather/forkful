---
title:                "C#: Å bruke regulære uttrykk"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor bruke regulære uttrykk? I en verden hvor data og informasjon er kongen, er det viktig å kunne håndtere store mengder tekst på en effektiv måte. Regulære uttrykk er et nyttig verktøy for å søke, matche og manipulere tekst på en konsistent og presis måte.

## Hvordan bruke regulære uttrykk i C#

Å bruke regulære uttrykk i C# kan virke skremmende til å begynne med, men det er egentlig ganske enkelt når du får taket på det. Først må du inkludere System.Text.RegularExpressions namespace i C#-filen din. Deretter kan du bruke følgende syntax for å søke etter et mønster i en tekststreng:

```C#
Regex regex = new Regex("mønster");
Match match = regex.Match("tekststreng");
```

Her vil "mønster" være ditt spesifikke søkemønster og "tekststreng" vil være teksten du ønsker å søke i. Du kan også bruke forskjellige metoder som Regex.Match(), Regex.Matches() og Regex.Replace() for å utføre forskjellige operasjoner på tekststrengen.

La oss si at du ønsker å finne alle ord som starter med en stor bokstav i en tekststreng. Da kan du bruke følgende regex-uttrykk:

```C#
Regex regex = new Regex(@"\b[A-Z]\w+");
MatchCollection matches = regex.Matches("Dette er en Test");
```

Her vil matches variabelen inneholde en liste over ord som starter med en stor bokstav, i dette tilfellet "Dette" og "Test". Du kan også bruke spesielle metakarakterer som ., *, + og {} for å søke etter mønstre som inneholder bestemte tegn.

## Dypdykk i regulære uttrykk

Regulære uttrykk er et kraftig verktøy, og det er mye dypere og mer komplekst enn bare å søke etter et enkelt mønster. Du kan bruke grupper og underuttrykk for å matche spesifikke deler av teksten, og du kan også bruke alternativer og lookarounds for å gjøre søkene dine mer presise.

Det finnes også en rekke nyttige verktøy og ressurser for å hjelpe deg med å lære og mestre regulære uttrykk, som for eksempel online regex-testingverktøy og bøker om emnet.

## Se også

- [Regex Tutorial](https://regexone.com/)
- [MSDN Documentation](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)