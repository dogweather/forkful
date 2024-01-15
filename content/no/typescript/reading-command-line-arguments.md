---
title:                "Lesing av kommandolinje-argumenter"
html_title:           "TypeScript: Lesing av kommandolinje-argumenter"
simple_title:         "Lesing av kommandolinje-argumenter"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvorfor lese kommandolinjeargumenter? Vel, hvis du noen gang har brukt et program med mange forskjellige funksjoner og innstillinger, er sjansene store for at du har brukt kommandolinjen for å kjøre programmet med de spesifikke innstillingene du ønsker. Å lese kommandolinjeargumenter er også nyttig for utviklere som ønsker å skrive programvare som kan bli brukt gjennom kommandolinjen, som for eksempel scripts eller automatiseringsverktøy.

## Slik gjør du det
Nå skal vi se på hvordan man kan lese kommandolinjeargumenter i TypeScript. Først må du importere "process" modulen fra Node.js. Deretter kan du bruke "process.argv" for å få tilgang til de forskjellige argumentene som er gitt ved kjøring av programmet. La oss ta en titt på et eksempel:

```TypeScript
import { process } from 'process';

console.log(process.argv);
```
Når du kjører dette programmet i terminalen med kommandoen "node filename.js arg1 arg2", vil output være følgende:
```shell
["node", "filename.js", "arg1", "arg2"]
```
Som du kan se, vil "process.argv" returnere en array med alle argumentene gitt ved kjøring av programmet. Det første elementet i arrayen vil alltid være "node", mens det andre elementet vil være navnet på filen som blir kjørt. De resterende elementene vil være de påfølgende argumentene som er gitt.

## Dypdykk
Det er noen ting å være oppmerksom på når man leser kommandolinjeargumenter. Hvis du ønsker å lese argumenter som inkluderer mellomrom, må du bruke anførselstegn ved kjøring av programmet. For eksempel ville argumentet "arg with space" bli gitt som "arg\ with\ space". I tillegg vil argumenter som starter med "-" bli sett på som flagg og ikke bli regnet som separate argumenter. Du kan også bruke "yargs" modulen for å håndtere argumenter med mer funksjonalitet og fleksibilitet.

## Se også
- [Node.js dokumentasjon for process.argv](https://nodejs.org/api/process.html#process_process_argv)
- [yargs - en npm modul for håndtering av kommandolinjeargumenter](https://www.npmjs.com/package/yargs)