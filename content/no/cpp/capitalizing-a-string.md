---
title:                "Gjøre en streng stor"
html_title:           "C++: Gjøre en streng stor"
simple_title:         "Gjøre en streng stor"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sette en streng til store bokstaver betyr å endre alle små bokstaver i en streng til store bokstaver. Dette gjøres ofte for å kjøre case-insensitive sammenligninger, parseIntput eller forbedre lesebarhet.

## Hvordan:
Her er en enkel metode for å endre en streng til store bokstaver ved hjelp av C++ Standard Template Library (STL):

```C++
#include <iostream>
#include <algorithm>
#include <string>

int main() {
    std::string str = "Hei, verden!";
    std::transform(str.begin(), str.end(), str.begin(), ::toupper);
    std::cout << str;
    return 0;
}
```
Når dette programmet kjøres, vil utdata være:

```C++
HEI, VERDEN!
```
## Dybdeplunge
Historisk sett er bruken av å sette til store bokstaver i programmeringssammenheng stammer fra tiden av trykte teletypemaskiner. I moderne praksis brukes det som nevnt for å ignorere case når du sammenligner strenger.

Det er flere andre måter å gjøre dette på i C++, for eksempel bruk av egen funksjon gjennom å løkke gjennom hver karakter i strengen og bruke `toupper` funksjonen.

I C++, når du bruker `std::transform` funksjonen, utføres operasjonen på plass, noe som betyr at den opprinnelige strengen blir endret. Hvis du vil beholde den opprinnelige strengen intakt, du vil trenge å kopiere strengen til en ny variabel før du setter den til store bokstaver.

## Se Også
- [C++ Standard Template Library (STL) Documentation](http://www.cplusplus.com/reference/stl/)
- [`std::transform` på cppreference](https://en.cppreference.com/w/cpp/algorithm/transform)
- [`toupper` på cppreference](https://en.cppreference.com/w/cpp/string/byte/toupper)