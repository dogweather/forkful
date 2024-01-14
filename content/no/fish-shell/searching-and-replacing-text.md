---
title:    "Fish Shell: Søking og erstattning av tekst"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Hvorfor

Å søke og erstatte tekst er en viktig del av programmering, spesielt når man jobber med store tekstfiler eller ønsker å gjøre flere endringer på en gang. Ved å bruke Fish Shell sine innebygde verktøy, kan du enkelt gjøre disse endringene uten å måtte åpne filen i et tekstredigeringsprogram.

## Slik gjør du det

For å søke og erstatte tekst i Fish Shell, kan du bruke kommandoen `sed` (stream editor). Dette er et kraftig verktøy som lar deg søke etter et mønster i en fil og erstatte med ønsket tekst.

La oss si at du ønsker å endre alle forekomster av ordet "katt" til "hund" i en fil. Her er et eksempel på hvordan du kan bruke `sed` i Fish Shell:

```Fish Shell
sed -i "s/katt/hund/g" fil.txt
```

I dette eksempelet erstatter `sed` alle forekomster av "katt" med "hund" i filen "fil.txt". Den `-i` flagget sørger for at endringene blir gjort direkte i filen, i stedet for å bare bli skrevet ut til terminalen.

For å gjøre søk og erstatting mer spesifikk, kan du også bruke regulære uttrykk. Dette lar deg finne mønstre i teksten i stedet for bare enkelte ord. Her er et eksempel på hvordan du kan bruke regulære uttrykk i `sed`:

```Fish Shell
sed -i "s/[0-9]+/X/g" tall.txt
```

I dette eksempelet erstatter `sed` alle tall i filen "tall.txt" med bokstaven "X". `[0-9]+` er et regulært uttrykk som betyr at `sed` skal lete etter en eller flere tall.

## Dypdykk

Når du bruker `sed` til å søke og erstatte tekst, er det viktig å forstå noen viktige konsepter. Først og fremst er `sed` ikke bare begrenset til å erstatte tekst, det kan også legge til eller slette tekst, samt formatere teksten på ulike måter.

En annen viktig ting å huske på er at `sed` bruker en "regexp" (regulær uttrykk) motor til å finne og erstatte tekst. Dette betyr at du kan lage veldig kraftige og presise mønstre for å finne tekst, men det kan også være litt vanskelig å lære.

For å lære mer om `sed` og bruk av regulære uttrykk, kan du se på disse ressursene:

- [Fish Shell sin offisielle dokumentasjon om `sed`](https://fishshell.com/docs/current/cmds/sed.html)
- [En guide til regulære uttrykk i `sed`](https://www.gnu.org/software/sed/manual/sed.html)
- [Et interaktivt verktøy for å lære regulære uttrykk](https://regexr.com/)

## Se også

- [En innføring i Fish Shell for nybegynnere](https://www.example.com)
- [Hvordan lage egne funksjoner i Fish Shell](https://www.example.com)
- [Brukbarheten til Fish Shell sammenlignet med andre shell-programmer](https://www.example.com)