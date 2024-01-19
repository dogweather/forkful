---
title:                "Interpolering av en streng"
html_title:           "Bash: Interpolering av en streng"
simple_title:         "Interpolering av en streng"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Interpolering av en streng betyr å flette variable inn i en streng. Programmerere gjør dette for å generere skreddersydd output, ofte for visning til brukere eller for logging.

## Hvordan:
Her er en grunnleggende måte å interpolere en streng på i C ++:

```C++
#include <iostream>
#include <string>

int main() {
    std::string navn = "Ola";
    int alder = 25;
    
    std::cout << "Hei, jeg er " << navn << ", og jeg er " << alder << " år gammel.\n";
    return 0;
}
```
Output:

```
Hei, jeg er Ola, og jeg er 25 år gammel.
```

## Dypdykk 
Historisk sett ble streng interpolering først introdusert i programmeringsspråk som Perl og Ruby. C ++ implementerer det på en mer manuell måte gjennom strømoperatoren `<<`. 

Alternativt kan vi også bruke `printf` eller `sprintf` for format-spesifikk interpolering, selv om det kan være mer feilsøkt. 

En annen teknikk er å bruke `std::format` fra C++20 som tillater strengformatering på en enklere og sikrere måte.

## Se også
1. [C++ String Interpolation using std::format (cppreference.com)](https://en.cppreference.com/w/cpp/utility/format)
2. [String Interpolation (Wikipedia)](https://en.wikipedia.org/wiki/String_interpolation)
3. [Properly formatted printf for Sstrings (stackoverflow.com)](https://stackoverflow.com/questions/2029103/properly-formatted-printf-for-strings)