---
title:                "Fish Shell: Skriving av tester"
simple_title:         "Skriving av tester"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor
Når vi skriver kode, ønsker vi alltid å sikre at den fungerer som forventet. Men hva skjer når vi endrer noe i koden i fremtiden? Vil det fortsatt fungere som det skal? Dette er hvor tester kommer inn i bildet. Ved å skrive tester, kan vi sørge for at koden vår alltid fungerer som den skal, selv etter at vi har gjort endringer.

## Hvordan
For å skrive tester i Fish Shell, bruker vi kommandoen "assert". Dette lar oss teste ulike deler av koden vår og sørge for at de oppfører seg som ønsket.

```Fish Shell
assert string length "Hei" -eq 3
```
I dette eksempelet sjekker vi om lengden på strengen "Hei" er lik 3. Dersom dette ikke stemmer, vil testen feile og gi en feilmelding.

En annen nyttig kommando er "contains", som kan sjekke om en streng inneholder en bestemt delstreng.

```Fish Shell
assert contains "Hei på deg" "på deg"
```
Her tester vi om strengen "Hei på deg" inneholder "på deg". Dersom dette er tilfelle, vil testen passere.

## Deep Dive
Det finnes mange forskjellige måter å skrive tester på i Fish Shell. Vi kan også teste om kommandoer kjører som forventet, eller om filer finnes. For mer informasjon om dette, kan du lese dokumentasjonen til Fish Shell eller ta en titt på noen av linkene nedenfor.

## Se også
- Fish Shell dokumentasjon: https://fishshell.com/docs/current/index.html
- En guide til testing i Fish Shell: https://fishshell.com/docs/current/tutorial.html#tests
- En samling av eksempler på testing i Fish Shell: https://github.com/fish-shell/fish-shell/tree/master/test/functions