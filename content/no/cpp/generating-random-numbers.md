---
title:                "C++: Lage tilfeldige tall"
simple_title:         "Lage tilfeldige tall"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor: Å generere tilfeldige tall i C++

Det er mange situasjoner hvor det er nyttig å kunne generere tilfeldige tall i programmering. Dette kan for eksempel være for å simulere ulike scenarioer, teste funksjonalitet, eller til og med skape et spill. Uavhengig av formål, kan tilfeldige tall legge til en ekstra dimensjon i koden din og gjøre den mer spennende.

## Hvordan: Eksempler på å generere tilfeldige tall i C++

For å generere tilfeldige tall i C++, bruker vi funksjonen `rand()` fra standardbiblioteket. Men før vi kan bruke denne, må vi inkludere biblioteket `cstdlib` og "seed" den tilfeldige tallgeneratoren vår med `srand()`.

Her er et enkelt eksempel på hvordan du kan generere et tilfeldig heltall mellom 1 og 10:

```C++
#include <iostream>
#include <cstdlib>
using namespace std;

int main() {
  // "Seed" den tilfeldige tallgeneratoren basert på nåværende tid
  srand(time(0));

  // Generer og skriv ut et tilfeldig tall mellom 1 og 10
  int tilfeldigTall = rand() % 10 + 1;
  cout << "Ditt tilfeldige tall er: " << tilfeldigTall << endl;

  return 0;
}
```

**Eksempel output:**
```
Ditt tilfeldige tall er: 6
```

## Deep Dive: Bakgrunn for å generere tilfeldige tall

Som nevnt tidligere, bruker vi funksjonen `rand()` for å generere tilfeldige tall i C++, men hva gjør egentlig denne funksjonen? I virkeligheten bruker den en "pseudorandom number generator" (PRNG) algoritme for å generere de tilfeldige tallene. Dette betyr at tallene ikke er helt tilfeldige, men følger en bestemt matematisk formel som gir et utvalg som er tilnærmet tilfeldig.

For å forbedre kvaliteten på de tilfeldige tallene, kan vi også velge å endre "seed" på den tilfeldige tallgeneratoren ved hjelp av funksjonen `srand()`. Dette vil gi en annen sekvens av tilfeldige tall hver gang programmet kjøres. For eksempel kan vi bruke verdien av nåværende tid som seed, som vist i eksempelet over.

## Se også

- [C++ standardbibliotek](https://www.cplusplus.com/reference/cstdlib/)
- [Hva er et pseudorandom number generator?](https://blog.bitsrc.io/what-is-a-pseudorandom-number-generator-prng-6f78b28543ec)
- [Tilfeldige tallgenerering i andre programmeringsspråk](https://www.geeksforgeeks.org/random-numbers-in-cpp/)