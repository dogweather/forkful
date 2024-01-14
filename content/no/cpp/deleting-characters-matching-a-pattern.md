---
title:                "C++: Sletting av tegn som matcher et mønster"
simple_title:         "Sletting av tegn som matcher et mønster"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor
Det kan være flere grunner til å ønske å slette karakterer som matcher et gitt mønster i et C++ program. Dette kan være for å utføre en spesifikk oppgave, forbedre koden eller for å lære mer om C++ og hvordan det håndterer strenger.

## Hvordan gjøre det
For å slette karakterer som matcher et gitt mønster, kan du bruke funksjonen `erase()` i C++ sammen med `find()` og `substr()`. La oss se på et eksempel:

```C++
#include <iostream>
#include <string>
using namespace std;

int main() {
    // Opprett en streng og et mønster
    string tekst = "Dette er en tekst";
    string mønster = "te";

    // Finn posisjonen til mønsteret i strengen
    size_t pos = tekst.find(mønster);

    // Slett karakterer som matcher mønsteret
    tekst.erase(pos, mønster.length());

    // Skriv ut resultatet
    cout << tekst << endl;

    return 0;
}
```

Denne koden vil finne og slette alle forekomster av mønsteret "te" i strengen "Dette er en tekst". Her er det viktig å merke seg at funksjonen `find()` returnerer posisjonen til den første forekomsten av mønsteret, og vi bruker denne posisjonen sammen med `erase()` for å slette mønsteret.

Output av dette programmet vil være "Dette er en sk".

## Dypdykk
Det er viktig å merke seg at funksjonen `find()` også tar inn en valgfri parameter for å angi startposisjonen for søket. Dette kan være nyttig hvis du ønsker å slette mønsteret fra en spesifikk posisjon i strengen. I tillegg kan du også bruke `replace()` funksjonen til å erstatte mønsteret med en annen streng.

Det finnes også andre metoder for å slette karakterer som matcher et gitt mønster, som for eksempel å iterere gjennom strengen og slette tegnene manuelt. Det er opp til deg å velge den beste metoden basert på dine behov og preferanser.

## Se også
- [C++ dokumentasjon - `erase()`](https://www.cplusplus.com/reference/string/string/erase/)
- [C++ dokumentasjon - `find()`](https://www.cplusplus.com/reference/string/string/find/)
- [C++ dokumentasjon - `substr()`](https://www.cplusplus.com/reference/string/string/substr/)