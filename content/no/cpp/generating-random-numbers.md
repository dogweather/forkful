---
title:    "C++: Generering av tilfeldige tall"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

Å generere tilfeldige tall kan være en nyttig funksjon i mange programmeringsprosjekter. Det kan være nyttig for å teste ulike scenarier, lage tilfeldige utvalg av data, eller for å legge til en tilfeldighetsfaktor i spill og simuleringer.

## Hvordan man gjør det

Genereringen av tilfeldige tall i C++ er enkelt med bruk av funksjoner fra standardbiblioteket `<random>`. Her er et eksempel på hvordan man kan generere et tilfeldig tall mellom 1 og 100:

```C++
#include <iostream>
#include <random>

int main() {
    // Initialiserer en tilfeldigt tallgenerator med seed fra systemtiden
    std::random_device rd;
    std::mt19937 gen(rd());
    // Definerer et fordelingsområde på 1-100
    std::uniform_int_distribution<> dis(1, 100);
    // Genererer et tilfeldig tall
    int random_num = dis(gen);
    // Skriver ut tilfeldig tall til konsollen
    std::cout << "Tilfeldig tall: " << random_num << std::endl;
}
```

Eksempel utdata:

```
Tilfeldig tall: 56
```

## Deep Dive

Det finnes flere ulike metoder for å generere tilfeldige tall i programmering. En vanlig metode er å bruke en pseudo-tilfeldig tallgenerator, som bruker en algoritme til å generere tall som tilsynelatende er tilfeldige. Det er viktig å huske at disse tallene ikke er helt tilfeldige, og at de kan følge en fordeling som kan forutsies.

For å sikre større tilfeldighet, kan man bruke en ekte tilfeldig tallgenerator som henter tall fra eksterne kilder som støy i radiosignaler eller målinger av fysiske fenomener som atmosfærisk støy. Disse tallene vil være mer tilfeldige, men kan være mer krevende å implementere.

## Se også

- [C++ standardbibliotek: <random>](https://en.cppreference.com/w/cpp/numeric/random)
- [Kilde til ekte tilfeldige tall: Random.org](https://www.random.org/)

Takk for at du leste! For mer informasjon om programmering i C++, sjekk ut vår blogg for flere artikler på norsk.