---
title:                "C++: Utskrift av feilsøkingsutdata"
simple_title:         "Utskrift av feilsøkingsutdata"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Debugging er en viktig del av programmering, og printing av debug output er en nyttig måte å feilsøke og finne feil i koden din på. Ved å printe ut verdier og variabler under kjøringen av programmet, kan du få en bedre forståelse av hva som skjer og hvor potensielle feil kan oppstå.

## Hvordan

For å printe debug output i C++, kan du bruke funksjonen `cout` fra `iostream`-biblioteket. Her er et eksempel på hvordan du kan printe ut en verdi og en streng i konsollen:

```C++
#include <iostream>

int main() {
    int num = 10;
    std::cout << "Verdien av num er: " << num << std::endl;
    return 0;
}
```

Dette vil gi følgende output i konsollen:
```
Verdien av num er: 10
```

Som du kan se, bruker vi `<<`-operatoren for å legge til verdier i outputen vår, og `std::endl` for å lage et linjeskift. Du kan også printe ut variabler eller verdier ved å bruke `printf`-funksjonen fra `cstdio`-biblioteket.

## Dypdykk

Det finnes flere forskjellige teknikker for å printe debug output i C++. Du kan for eksempel bruke `cerr` for å printe ut feilmeldinger, eller `clog` for å printe ut mer informasjon under kjøringen av programmet. Det er også mulig å formatere outputen ved hjelp av manipulatorer som `setw()` og `setprecision()`.

Det er viktig å være forsiktig med å bruke for mye debug output, da det kan gjøre koden din unødvendig rotete og påvirke ytelsen til programmet ditt. Derfor kan det være lurt å bruke preprosessor-direktiver, som `#ifdef` og `#ifndef`, for å deaktivere debug output når du er ferdig med å feilsøke og teste koden din.

## Se også
- [Dealing with Debug Output in C++](https://devblogs.microsoft.com/cppblog/dealing-with-debug-output-in-c/)
- [Using cout in C++](https://www.geeksforgeeks.org/using-cout-cpp/)
- [Debugging and Error Handling in C++](https://www.techinfected.net/2019/02/debugging-and-error-handling-in-cpp.html)