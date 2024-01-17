---
title:                "Generering av tilfeldige tall"
html_title:           "C++: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva er det og hvorfor gjør vi det?
Å generere tilfeldige tall er en vanlig oppgave for programmerere. Det betyr rett og slett å lage tall som er tilfeldig valgt innenfor et bestemt område eller et sett av regler. Dette kan være nyttig for å simulere situasjoner, lage spill, eller for sikkerhetsformål.

## Hvordan gjør vi det:
I C++ kan vi generere tilfeldige tall ved hjelp av funksjonen `rand()`. Denne funksjonen genererer en tilfeldig tallverdi mellom 0 og `RAND_MAX` (en konstant definert i `<cstdlib>`). For å begrense området til et bestemt intervall, kan vi bruke `%` operatøren. Her er et eksempel som genererer et tilfeldig tall mellom 1 og 10:

```C++
#include <cstdlib>
#include <ctime>

int main() {
    // Sett en seed for generering av tilfeldige tall basert på nåværende tid
    srand(time(0));

    // Generer et tilfeldig tall mellom 1 og 10
    int tilfeldigTall = rand() % 10 + 1;
    
    // Skriv ut resultatet
    std::cout << "Det tilfeldige tallet er: " << tilfeldigTall;
}
```

Dette eksempelet bruker også funksjonen `srand()` for å sette en "seed" for genereringen av tilfeldige tall. Dette er viktig for å sikre at tallene blir mer tilfeldige hver gang programmet kjøres.

## Dypdykk:
Historisk sett har generering av tilfeldige tall vært en vanskelig oppgave for programmerere. Tidligere ble det brukt komplekse matematiske formler eller til og med fysiske prosesser, som for eksempel å kaste terninger, for å få tilfeldige tall. Med fremveksten av datamaskiner har det blitt enklere å generere tilfeldige tall ved hjelp av algoritmer.

I tillegg til `rand()` funksjonen, finnes det også andre metoder for å generere tilfeldige tall i C++. Et alternativ er å bruke biblioteket `<random>` som tilbyr mer avanserte metoder for å generere tilfeldige tall, som for eksempel fordelingsfunksjoner og tilpassede fordelinger.

Det er også viktig å huske at selv om funksjonen `rand()` genererer tall som kan virke tilfeldige, er de egentlig deterministiske. Det vil si at hvis vi bruker samme seed, vil vi få de samme tallene hver gang programmet kjøres. Derfor er det viktig å bruke en god seed, som for eksempel nåværende tid, for å få mer tilfeldige tall.

## Se også:
- [C++ rand() funksjon referanse](https://www.cplusplus.com/reference/cstdlib/rand/)
- [C++ biblioteket <random> referanse](https://www.cplusplus.com/reference/random/)