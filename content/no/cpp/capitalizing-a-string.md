---
title:                "Sette streng til store bokstaver"
date:                  2024-01-19
html_title:           "Arduino: Sette streng til store bokstaver"
simple_title:         "Sette streng til store bokstaver"

category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Capitalizing a string betyr å gjøre det første tegnet i hvert ord stort, som i titler eller starten av setninger. Programmerere bruker dette for å forbedre lesbarheten og sikre konsistent tekstformat.

## Hvordan gjøre det:
Her er et raskt eksempel på hvordan du kan gjøre om en streng til bare store bokstaver i C++ med funksjonen `std::transform` og `::toupper`.

```C++
#include <iostream>
#include <string>
#include <algorithm>
#include <cctype>

int main() {
    std::string text = "halla norge";
    std::transform(text.begin(), text.end(), text.begin(),
                   [](unsigned char c) { return std::toupper(c); });

    std::cout << "Capitalized String: " << text << std::endl;  
    return 0;
}
```

Sample output:
```
Capitalized String: HALLA NORGE
```

Hvis du ønsker kun første bokstav i hvert ord stort, kan du tweake koden litt:

```C++
#include <iostream>
#include <string>
#include <cctype>

void CapitalizeFirstLetter(std::string &s) {
    bool cap = true;
    for (char &c : s) {
        if (isspace(c)) {
            cap = true;
        } else if (cap && islower(c)) {
            c = toupper(c);
            cap = false;
        }
    }
}

int main() {
    std::string text = "welcome to oslo";
    CapitalizeFirstLetter(text);
    std::cout << "Capitalized String: " << text << std::endl;
    return 0;
}
```

Sample output:
```
Capitalized String: Welcome To Oslo
```

## Dypdykk
Tidligere var det vanlig å bruke for-løkker og manuell manipulasjon av tegn for å endre store og små bokstaver i C++-strenger. Nå foretrekker vi standardbibliotekfunksjoner som `std::transform` og `::toupper`. Alternativer til `::toupper` inkluderer å bruke ASCII-verdier direkte for å utføre transformasjoner, men dette er ikke anbefalt da det kan bli ulesbart og feilutsatt.

`std::transform` er en del av standardbiblioteket som ble introdusert i C++98 og har blitt en del av kodevanen for mange C++ utviklere siden da. Bruk av lambda-uttrykk for å definere funksjoner on-the-fly, som vist i eksempelet ovenfor, ble introdusert i C++11 og gjør koden mer kompakt og lettlest.

Det er også viktig å merke seg at `::toupper` er en C-bibliotekfunksjon og krever `#include <cctype>`. Det er en type-safe versjon i C++ standardbiblioteket, `std::toupper`, som kan være å foretrekke i strengt typekontrollerte situasjoner.

Hvorvidt man velger å bruke slike innebygde funksjoner eller utarbeide sin egen løsning, avhenger av det spesifikke bruksområdet og kravene i prosjektet. For eksempel, hvis man trenger å vekte fremfor ytelse, kan man velge en mer detaljert og finjustert tilnærming.

## Se Også
- [cppreference std::transform](https://en.cppreference.com/w/cpp/algorithm/transform)
- [cppreference std::toupper](https://en.cppreference.com/w/cpp/string/byte/toupper)
