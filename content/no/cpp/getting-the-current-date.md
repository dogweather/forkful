---
title:                "C++: Hente gjeldende dato"
simple_title:         "Hente gjeldende dato"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Å få den nåværende datoen kan være en viktig del av mange programmeringsoppgaver. Dette kan være nyttig for å lagre tidsstempler på data, beregne forsinkelser eller bare for å vise brukeren den nåværende datoen.

## Slik gjør du

```C++
#include <iostream>
#include <ctime>

int main() {
    // Få nåværende tidspunkt
    time_t now = time(0);

    // Konverter tidspunktet til en streng
    char* dt = ctime(&now);

    // Skriv ut nåværende dato
    std::cout << "Dagens dato er: " << dt << std::endl;

    return 0;
}
```
**Output:** Dagens dato er: Tue Dec 08 09:20:45 2020

Det første trinnet er å inkludere hodet ```<ctime>```, som inneholder funksjoner for å håndtere tid og dato i C++. Vi starter deretter med å deklarere en variabel ```now``` av typen ```time_t```, som vil holde den nåværende tidspunktet.

Den neste linjen kaller funksjonen ```time``` med parameteren ```0```, som returnerer antall sekunder siden 1. januar 1970. Dette tilsvarer den nåværende datoen og klokkeslettet.

Vi bruker deretter funksjonen ```ctime``` til å konvertere ```time_t```-verdien til en streng som kan leses av mennesker. Denne strengverdien lagres i en variabel ```dt```.

Til slutt skriver vi ut verdien av ```dt```, som inneholder den nåværende datoen og klokkeslettet.

## Dykk dypere

Det finnes flere andre metoder for å få den nåværende datoen i C++. En annen metode er å bruke funksjonen ```localtime```, som også gir mulighet for å formatere datoen og klokkeslettet.

Det finnes også biblioteker som ```boost::date_time``` som gir mer avanserte funksjoner for håndtering av tid og dato i C++.

## Se også

- [cplusplus.com - Date and Time Library](https://www.cplusplus.com/reference/ctime/)
- [cppreference.com - <ctime> header](https://en.cppreference.com/w/cpp/header/ctime)
- [Programiz - C++ Date and Time](https://www.programiz.com/cpp-programming/library-function/ctime)