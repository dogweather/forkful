---
title:                "Å skrive tester"
html_title:           "Bash: Å skrive tester"
simple_title:         "Å skrive tester"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor
Å skrive tester er en viktig del av Bash-programmering for å sikre at koden din fungerer som den skal. Det bidrar til å identifisere og fikse feil tidligere i utviklingsprosessen og sikrer at koden din er pålitelig og robust når den blir implementert.

## Slik gjør du det
For å skrive tester i Bash, bruker du kommandoen ```test``` eller dobbel parenteser (( )). La oss si at vi vil teste om en variabel er lik 10. Vi kan gjøre det ved å skrive følgende:

```
test $variabel -eq 10

(( $variabel == 10 ))
```

Hvis variabelen er lik 10, vil testen returnere "true" og sette exit-verdien til 0. Hvis variabelen er forskjellig fra 10, vil testen returnere "false" og sette exit-verdien til 1.

Du kan også bruke tester til å sjekke om en fil eksisterer, om en streng er tom eller om to strenger er like. Se Bash-dokumentasjonen for en fullstendig liste over tester og deres syntaks.

## Dypdykk
Når du skriver tester, er det viktig å være nøyaktig med syntaksen. En enkelt feil kan føre til uforutsette resultater, så sørg for å dobbeltsjekke testene dine.

Det er også viktig å organisere testene dine på en måte som gjør det enkelt å feilsøke. Du kan opprette en egen test-funksjon og kalle den i din hovedfunksjon, for eksempel.

Videre kan du også bruke assert-kommandoer for å sjekke om testene dine gir forventede resultater. Dette gjør det enklere å finne feil hvis en test ikke går som forventet.

## Se også
- [Bash-dokumentasjonen for tests](https://linux.die.net/man/1/test)
- [Bash Guide for nybegynnere](https://tldp.org/LDP/Bash-Beginners-Guide/html/index.html)
- [Enkel guide til Bash-testing fra CodingAlpha](https://www.codingalpha.com/bash-shell-scripting-test/)