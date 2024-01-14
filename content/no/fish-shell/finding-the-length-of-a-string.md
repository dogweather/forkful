---
title:                "Fish Shell: Å finne lengden på en streng"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor
Å finne lengden til en streng er en vanlig oppgave innen programmering. Det kan være nyttig for å validere bruker input eller for å bearbeide tekst data. I denne bloggposten skal jeg vise deg hvordan du kan gjøre dette ved hjelp av Fish Shell.

## Hvordan
For å finne lengden til en streng i Fish Shell, kan du bruke kommandoen "string length". Her er et eksempel på hvordan du kan bruke kommandoen og få utskrift av lengden:

```Fish Shell
set mystr "Dette er en streng"
echo (string length $mystr)
```
Output: 19

I dette eksemplet brukte vi variabelen "mystr" for å lagre strengen "Dette er en streng". Deretter brukte vi "string length" kommandoen for å finne lengden til strengen og deretter skrev den ut ved hjelp av "echo" kommandoen.

Du kan også bruke "string length" direkte på en streng uten å bruke en variabel. Her er et eksempel:

```Fish Shell
echo (string length "Jeg elsker å programmere!")
```
Output: 24

## Deep Dive
Det som skjer bak kulissene når vi bruker "string length" kommandoen, er at Fish Shell teller antall tegn i strengen ved å iterere gjennom den og telle hver bokstav. Dette betyr at tomme mellomrom og spesialtegn også vil bli talt.

Det er også verdt å nevne at "string length" kommandoen kan brukes på både enkeltstående ord og hele setninger. I begge tilfeller vil kommandoen gi utskrift av lengden på strengen.

## Se også
- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/cmds/string.html)
- [Hvordan bruke variabler i Fish Shell](https://fishshell.com/docs/current/tutorial.html#variables)
- [Eksempler på utfordringer du kan løse med Fish Shell](https://blog.christianvarga.com/fish-shell-features-youre-missing-out-on/)