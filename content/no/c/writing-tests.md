---
title:                "Skriver tester"
html_title:           "C: Skriver tester"
simple_title:         "Skriver tester"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/writing-tests.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å skrive tester er en viktig del av å være en C programmerer. Det er en måte å forsikre deg om at koden din fungerer som det skal og å sjekke for eventuelle feil før du sender den til produksjon. Tester hjelper deg også med å forstå koden din bedre og gjør det enklere å feilsøke hvis noe går galt senere.

## Hvordan:
For å skrive tester i C, bruker vi et bibliotek kalt "assert.h". Dette biblioteket lar oss skrive utsagn som sjekker om en betingelse er sann eller falsk. La oss se på et eksempel:

```C
#include <assert.h>
#include <stdio.h>

int main() {
    int num = 5;

    // Sjekker om num er lik 5
    assert(num == 5);

    printf("%d er lik 5\n", num);
    return 0;
}
```
Dette eksempelet bruker "assert" -funksjonen til å sjekke om variabelen "num" er lik 5. Hvis den er det, vil programmet fortsette å kjøre som normalt, men hvis den ikke er det, vil programmet krasje og gi deg en feilmelding. Dette gjør det enkelt å oppdage og fikse eventuelle feil i koden din.

## Dykk dypere:
Historisk sett ble tester skrevet manuelt og var en tidkrevende prosess. I dag finnes det mange verktøy og biblioteker som gjør testprosessen mye enklere og mer effektiv. Noen av disse inkluderer "check" og "CppUTest". Disse verktøyene gir mer avanserte tester og lar deg organisere dem i forskjellige kategorier.

En annen alternativ tilnærming er å bruke "TDD" - testdrevet utvikling. Dette er en utviklingsmetodikk hvor du skriver testene først, og deretter skriver koden som oppfyller disse testene. Dette kan hjelpe deg med å skrive mer pålitelig og feilfri kode.

Når du skriver tester, er det viktig å huske å teste både positive og negative scenarier. Tenk på mulige feil som kan oppstå og skriv tester for å håndtere dem.

## Se også:
- [CppUTest](https://cpputest.github.io/)
- [Check](https://libcheck.github.io/check/)
- [Testdrevet utvikling](https://en.wikipedia.org/wiki/Test-driven_development)