---
title:                "Generering av tilfeldige tall"
date:                  2024-01-20T17:48:34.932898-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generering av tilfeldige tall"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

I C++ er det å generere tilfeldige tall viktig for å skape uforutsigbarhet eller testing. Programmerere bruker tilfeldighet i spill, simuleringer og for å velge data tilfeldig.

## How to:

La oss sjekke ut hvordan du lager tilfeldige tall i C++. Det er enkelt:

```C++
#include <iostream>
#include <random>

int main() {
    // Opprett en generator
    std::mt19937 generator(static_cast<long unsigned int>(time(0)));

    // Definer intervallet
    std::uniform_int_distribution<int> distribution(1, 100);

    // Generer et tilfeldig tall
    int tilfeldigTall = distribution(generator);

    // Skriv ut det tilfeldige tallet
    std::cout << "Tilfeldig tall: " << tilfeldigTall << std::endl;

    return 0;
}
```

Kjører du dette, får du et tall mellom 1 og 100. Enkelt og greit.

## Deep Dive

Før C++11, var `<cstdlib>` og `rand()` go-to for tilfeldige tall. Ikke så bra, for å være ærlig. De var ikke så tilfeldige som de burde være og hadde en tendens til å lage mønstre. 

C++11 kom med `<random>`, en game-changer. Plutselig fikk vi kvalitetsgeneratorer som `std::mt19937`, en Mersenne Twister, og bedre fordelinger som `std::uniform_int_distribution`. Enkel bruk og ekte tilfeldighet ble en realitet.

Men vi har andre alternativer også. For eksempel, bruk `std::random_device` som en seed for ekstra entropi. Eller sjekk ut tredjepartsbiblioteker hvis standardbiblioteket ikke holder mål for dine behov.

## See Also:

- C++ Standards Committee papers: https://isocpp.org/
- cppreference.com for å dykke dypere i `<random>`: https://en.cppreference.com/w/cpp/header/random
- Stack Overflow for spørsmål og svar om tilfeldige tall i C++: https://stackoverflow.com/questions/tagged/random+c%2b%2b