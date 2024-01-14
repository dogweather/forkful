---
title:    "C++: Å bruke regulære uttrykk"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Hvorfor bruke regulære uttrykk i C++?

Regulære uttrykk er en viktig del av programmeringsverktøykassen til enhver profesjonell utvikler. De gir et effektivt og nøyaktig verktøy for å søke og manipulere tekststrenger innenfor et program. Med regulære uttrykk kan du enkelt finne og behandle ulike mønstre i tekst, som for eksempel e-postadresser, telefonnummer eller variabelnavn. Dette gjør det mye lettere å håndtere store mengder data og automatisere oppgaver i C++.

## Hvordan bruke regulære uttrykk i C++?

Det første steget for å kunne bruke regulære uttrykk i C++ er å inkludere biblioteket `<regex>` i koden. Dette gir tilgang til de nødvendige funksjonene og klassene for å lage og bruke regulære uttrykk.

Her er et enkelt eksempel på hvordan du kan bruke regulære uttrykk i C++:

```C++
#include <iostream>
#include <regex>

int main() {
    // Definerer et regulært uttrykk som finner alle tall i en tekststreng
    std::regex reg("(\\d+)");

    // Tekststreng for å søke i
    std::string tekst = "Jeg er 25 år gammel.";

    // Bruker std::sregex_iterator for å finne alle forekomster av uttrykket
    std::sregex_iterator it(tekst.begin(), tekst.end(), reg);
    std::sregex_iterator end;

    // Går gjennom alle forekomster og skriver ut resultatene
    while (it != end) {
        std::smatch match = *it;
        std::cout << match.str() << std::endl;
        it++;
    }

    return 0;
}
```

Dette eksempelet vil skrive ut tallet 25 i konsollen. Her brukes funksjonen `std::sregex_iterator` til å finne alle forekomster av det definerte regulære uttrykket i tekststrengen.

## Dypdykk i bruk av regulære uttrykk i C++

Regulære uttrykk i C++ er basert på Perl-versjonen av regulære uttrykk og følger samme syntaks. Dette gjør det enklere for de som allerede er kjent med regulære uttrykk fra andre programmeringsspråk.

I C++ finnes det ulike typer uttrykk som du kan bruke for å søke og manipulere tekst. Noen av de mest nyttige er:

- `\\d` - matche et hvilket som helst tall
- `\\w` - matche en bokstav, tall eller understrek
- `[ ]` - definere en karakterklasse, for eksempel `[abc]` matcher a, b eller c
- `+` - matche en eller flere forekomster av den forrige karakteren eller karakterklassen
- `*` - matche null eller flere forekomster av den forrige karakteren eller karakterklassen
- `^` - matche begynnelsen på en linje
- `$` - matche slutten på en linje

Det er også mulig å bruke regulære uttrykk for å erstatte deler av en tekststreng eller holde styr på grupper av tekst. Det finnes mange fler funksjoner og muligheter med regulære uttrykk i C++, og det er verdt å utforske dem nærmere for å forbedre dine programmeringsferdigheter.

# Se også

- <https://en.cppreference.com/w/cpp/regex>
- <https://www.regular-expressions.info/>
- <https://www.codesdope.com/cpp-regular-expressions/>