---
title:                "C++: Stor bokstav i en streng"
simple_title:         "Stor bokstav i en streng"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Hvorfor

Uansett om du er en erfaren programmerer eller bare begynner å lære C++, kan det være nyttig å vite hvordan man kan kapitalisere en streng i koden din. Enten det er for å formatere utdataen eller behandle brukerinput, er det ofte nødvendig å gjøre en tekststreng til store bokstaver. I denne bloggposten vil vi utforske hvordan man kan gjøre dette på en enkel måte.

## Hvordan gjøre det

Hvis du ønsker å kapitalisere en streng, slik at alle bokstavene er store, har C++ allerede en innebygd funksjon for dette. Funksjonen heter `toupper()` og den kan brukes ved å inkludere `#include <cctype>` i koden din. For å kapitalisere en streng, må du først hente inn strengen med `std::string`, og deretter bruke en for-løkke for å gå gjennom hver bokstav og bruke `toupper()` på hver bokstav ved hjelp av ASCII-verdiene. Her er et eksempel på hvordan det kan gjøres:

```C++
#include <iostream>
#include <cctype>
#include <string>

using namespace std;

int main() {

    // Opprett en string
    string tekst = "dette er en tekststreng";

    // Loop gjennom hver bokstav og bruk toupper() for å gjøre den stor
    for (int i = 0; i < tekst.length(); i++) {
        tekst[i] = toupper(tekst[i]);
    }

    // Skriv ut den kapitaliserte strengen
    cout << tekst << endl;

    return 0;
}
```

Eksempelutdata:

```
DETTE ER EN TEKSTSTRENG
```

I dette eksempelet brukte vi `tekst.length()` for å få lengden på strengen, og deretter `toupper()` på hver bokstav ved hjelp av indeksering `tekst[i]`.

## Dypdykk

Når man bruker `toupper()` funksjonen, er det viktig å huske at den bare fungerer på bokstaver fra A til Z. Bokstaver som æ, ø og å vil ikke bli endret til store bokstaver. Hvis du ønsker å inkludere disse bokstavene, kan du enten bruke `setlocale()` funksjonen eller sjekke ASCII-verdien til hver bokstav og endre den manuelt.

En annen ting å huske på er at `toupper()` funksjonen returnerer ASCII-verdien for den store bokstaven, så hvis du ønsker å få ut en faktisk bokstav, må du konvertere ASCII-verdien tilbake til en `char` verdi.

## Se også

- [C++ strings](https://en.cppreference.com/w/cpp/string)
- [String functions in C++](https://www.geeksforgeeks.org/string-functions-in-cpp/)
- [ASCII table](https://www.ascii-code.com/)