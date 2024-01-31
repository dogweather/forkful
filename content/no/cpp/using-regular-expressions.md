---
title:                "Bruk av regulære uttrykk"
date:                  2024-01-19
simple_title:         "Bruk av regulære uttrykk"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Regulære uttrykk er mønstre brukt for å matche tekst. De er essensielle for søking, validering og tekstmanipulasjon, fordi de effektiviserer og automatiserer kompleks tekstbehandling.

## How to:
```cpp
#include <iostream>
#include <regex>
using namespace std;

int main() {
    // Definerer et regex-mønster for e-postvalidering
    regex epost_mønster(R"([a-zA-Z0-9\.-_]+@[a-zA-Z0-9\.-]+\.[a-zA-Z]{2,})");

    // Tekststreng som skal sjekkes
    string tekst = "Min e-post er eksempel@domene.no";

    // Søker etter mønsteret i teksten
    smatch resultat;
    if(regex_search(tekst, resultat, epost_mønster)) {
        cout << "Fant en e-post: " << resultat[0] << endl;
    } else {
        cout << "Ingen gyldig e-post funnet." << endl;
    }

    return 0;
}
```
Output:
```
Fant en e-post: eksempel@domene.no
```

## Deep Dive
Regulære uttrykk oppsto på 1950-tallet og har utviklet seg til et kraftig verktøy innen programmering. Alternativer som tekststrengsfunksjoner (som `find`, `substring`) eksisterer, men mangler fleksibiliteten til regulære uttrykk. C++ bruker `<regex>` biblioteket for å implementere dette, og det følger POSIX- og Perl-kompatible mønsterstandarder.

## See Also
- [cppreference.com Regex library](https://en.cppreference.com/w/cpp/regex)
- [Regular Expressions Quick Start](https://www.regular-expressions.info/quickstart.html)
