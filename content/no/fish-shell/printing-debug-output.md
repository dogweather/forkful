---
title:                "Utskrift av feilsøkingsutdata"
html_title:           "Fish Shell: Utskrift av feilsøkingsutdata"
simple_title:         "Utskrift av feilsøkingsutdata"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å skrive ut feilsøkingsutdata betyr i hovedsak å vise informasjon om hva som skjer i programmet ditt når det kjører. Dette kan være nyttig for å forstå hvordan programmet fungerer og for å finne og fikse feil. Programmerere bruker dette for å løse problemer og forbedre koden sin.

## Slik gjør du det:

```
Fish Shell har et enkelt og effektivt kommando for å skrive ut feilsøkingsutdata: `echo`. Her er et eksempel:

$ set var1 'hello'
$ set var2 'world'
$ echo var1 var2
hello world
```

## Dykk dypere:

Feilsøkingsutdata har blitt brukt av programmerere siden kodetidens begynnelse. I dag er det flere alternativer for å skrive ut feilsøkingsutdata, som for eksempel å bruke loggingbiblioteker eller å bruke utviklerverktøy i nettleseren. Å bruke `echo` kommandoen i Fish Shell er raskt og enkelt, men det kan også være nyttig å undersøke andre alternativer hvis du har et større prosjekt eller en mer kompleks feilsøking.

## Se også:

[Fish Shell dokumentasjon](https://fishshell.com/docs/current/)

[Debugging med Echo](
https://www.hostinger.no/tutorials/how-to-debug-in-fish-shell/)