---
title:                "Generere tilfeldige tall"
html_title:           "Arduino: Generere tilfeldige tall"
simple_title:         "Generere tilfeldige tall"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å generere tilfeldige tall er en prosess der datamaskinen lager tall som ikke kan forutsies av brukeren. Programmerere gjør dette for å legge til uforutsigbarhet i spill, testing av programvare, og simulering av forskjellige scenarioer.

## Hvordan gjør man det?

Her er en enkel kode på hvordan du genererer et tilfeldig tall mellom 1 og 10 i C++:

```C++
#include <random>
#include <iostream>

int main()
{
    std::random_device rd;
    std::mt19937 mt(rd());
    std::uniform_int_distribution<> dist(1, 10);

    std::cout << dist(mt) << '\n';   

    return 0;
}
```
Dette programmet bruker `std::random_device` for å skape et startpunkt (eller "seed") for generatoren `std::mt19937`, som så brukes sammen med `std::uniform_int_distribution<>` for å generere et tilfeldig tall.

## Deep Dive

Historisk, mange gamlinger har brukt en enkel metode kalt `rand()`, men denne metoden gir ikke nødvendigvis en jevn distribusjon av tall og kan være forutsigbar. Når vi bruker `std::uniform_int_distribution<>` sammen med en god generator som `std::mt19937`, får vi en bedre distribusjon av tall.

Alternativer til `std::mt19937` inkluderer `std::minstd_rand` og `std::ranlux48`, som har et litt annet balanse mellom kvalitet, hastighet, og minnebruken.

Når det gjelder implementering, er random-number generators (RNGs) designet for å generere sekvenser av tall som i det minste ser tilsynelatende tilfeldig ut. Å lage en god RNG kan være veldig vanskelig - det er lett å lage en som ser tilfeldig ut ved første øyekast, men som har mønstre eller svakheter som blir åpenbare når den brukes mye.

## Se Også

Her er noen lenker til litt mer om generering av tilfeldige tall i C++:

1. [CPP Reference - Random](http://en.cppreference.com/w/cpp/numeric/random)
2. [CPlusPlus.com - Random](http://www.cplusplus.com/reference/random/)
3. [StackOverflow - What is the best random number generator in C++?](https://stackoverflow.com/questions/39288552/what-is-the-best-random-number-generator-in-c)
4. [GeeksForGeeks - Random number generation in C++](https://www.geeksforgeeks.org/random-number-generation-in-cpp/)