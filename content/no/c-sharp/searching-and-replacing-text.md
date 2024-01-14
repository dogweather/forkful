---
title:                "C#: Søking og bytte av tekst"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Å søke og erstatte tekst er en viktig del av enhver programmeringsoppgave. Enten du skal endre en enkelt linje i en fil eller erstatte tekst i flere filer samtidig, er det en effektiv måte å spare tid og sikre nøyaktighet i koden din.

## Slik gjør du det

Følg disse enkle trinnene for å søke og erstatte tekst i C#:

1. Åpne Visual Studio og åpne prosjektet du ønsker å endre tekst i.
2. Gå til "Edit" -menyen og velg "Find and Replace" (Søk og erstatt).
3. I søkefeltet, skriv inn teksten du ønsker å erstatte.
4. I erstatningsfeltet, skriv inn den nye teksten du ønsker å erstatte den gamle teksten med.
5. Velg om du vil søke i hele prosjektet, gjeldende dokument eller en gruppe dokumenter.
6. Klikk på "Replace" (Erstatt) for å erstatte teksten én gang, eller "Replace All" (Erstatt alt) for å erstatte all forekomst av teksten.
7. Gjenta trinnene for å søke og erstatte mer tekst om nødvendig.

```C#
// Eksempel på søke og erstatte tekst:
string originalTekst = "Hei, verden!";
string nyTekst = "Hallo, verden!";
string endretTekst = originalTekst.Replace("Hei", "Hallo");

Console.WriteLine(endretTekst);
// Output: Hallo, verden!
```

## Dykk dypere

Når du skal gjøre mer avanserte søk og erstatte-operasjoner, kan det være nyttig å bruke regulære uttrykk. Dette gjør det mulig å finne og erstatte tekst basert på spesifikke mønstre og ikke bare en enkelt streng.

For å bruke regulære uttrykk i C#, må du importere System.Text.RegularExpressions-namespace og bruke klassen Regex. Deretter kan du bruke metoder som Match og Replace for å finne og erstatte tekst basert på mønstre.

## Se også

- [RegEx Tutorial – En guide til regulære uttrykk i C#](https://www.tutorialspoint.com/csharp/csharp_regular_expressions.htm)
- [Søke og erstatte tekst i Visual Studio](https://docs.microsoft.com/en-us/visualstudio/ide/quickstart-search-and-replace?view=vs-2019)