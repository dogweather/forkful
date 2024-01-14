---
title:                "C++: Sammenslåing av strenger"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

# Hvorfor

Å kombinere strenger er en viktig del av programmering, spesielt i C++. Det lar oss lage dynamiske og varierte tekster som kan tilpasses basert på ulike forhold. I denne bloggposten vil vi lære hvorfor og hvordan man får til dette i C++.

## Hvordan

Det er mange måter å kombinere strenger på i C++, men den mest vanlige er ved å bruke operatorer eller funksjoner. La oss se på et eksempel på hvordan vi kan bruke operatorer til å kombinere strenger:

```C++
#include <iostream>
#include <string>

int main() {
    std::string fornavn = "John";
    std::string etternavn = "Smith";
    std::cout << fornavn + " " + etternavn << std::endl;
    return 0;
}
```
**Resultat:** John Smith

Vi kan også bruke en funksjon kalt `append()` for å legge til tekst til slutten av en eksisterende string:

```C++
#include <iostream>
#include <string>

int main() {
    std::string adjektiv = "fantastisk";
    std::string setning = "Det er en ";
    setning.append(adjektiv);
    std::cout << setning << std::endl;
    return 0;
}
```
**Resultat:** Det er en fantastisk

Hvis du vil legge til tall til en string, kan du bruke `std::to_string()`-funksjonen:

```C++
#include <iostream>
#include <string>

int main() {
    int alder = 25;
    std::string tekst = "Jeg er " + std::to_string(alder) + " år gammel.";
    std::cout << tekst << std::endl;
    return 0;
}
```
**Resultat:** Jeg er 25 år gammel.

## Deep Dive

Nå som vi har sett noen eksempler på hvordan man kan kombinere strenger i C++, la oss se på hvordan det egentlig fungerer bak kulissene. I C++, er strenger bare en sekvens av tegn lagret i minnet. Når vi bruker operatoren `+` eller `append()`-funksjonen, blir den originale strengen kopiert inn i en ny streng, og så legges den nye teksten til på slutten. Dette kan føre til en del unødvendig bruk av minne og kan være ineffektivt for store strenger.

En bedre måte å kombinere strenger på er ved å bruke `std::stringstream`-klasse, som lar oss bygge en string bit for bit uten å lage flere kopier av den originale teksten. La oss se på et eksempel:

```C++
#include <iostream>
#include <sstream>

int main() {
    std::stringstream ss;
    std::string fornavn = "John";
    std::string etternavn = "Smith";
    int alder = 25;

    ss << fornavn << " " << etternavn << " er " << alder << " år gammel.";
    std::cout << ss.str() << std::endl;
    return 0;
}
```
**Resultat:** John Smith er 25 år gammel.

Ved å bruke `std::stringstream`, kan vi kombinere ulike datatyper og tekst uten å måtte lage flere kopier av den originale strengen.

## Se Også

- [C++ string concatenation tutorial](https://www.geeksforgeeks.org/concatenate-two-strings-in-cpp/)
- [C++ stringstream tutorial](https://www.geeksforgeeks.org/stringstream-c-applications/)
- [Stack Overflow: Best way to concatenate strings in C++](https://stackoverflow.com/questions/20430772/best-way-to-concatenate-strings-in-c)