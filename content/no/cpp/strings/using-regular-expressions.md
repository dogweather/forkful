---
aliases:
- /no/cpp/using-regular-expressions/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:15.392420-07:00
description: "Regul\xE6re uttrykk i C++ er sekvenser av tegn som definerer et s\xF8\
  kem\xF8nster, brukt for strengs\xF8king eller manipulasjon. Programmerere bruker\
  \ dem til oppgaver\u2026"
lastmod: 2024-02-18 23:08:54.174527
model: gpt-4-0125-preview
summary: "Regul\xE6re uttrykk i C++ er sekvenser av tegn som definerer et s\xF8kem\xF8\
  nster, brukt for strengs\xF8king eller manipulasjon. Programmerere bruker dem til\
  \ oppgaver\u2026"
title: "Bruke regul\xE6re uttrykk"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Regulære uttrykk i C++ er sekvenser av tegn som definerer et søkemønster, brukt for strengsøking eller manipulasjon. Programmerere bruker dem til oppgaver som å validere inndata, søke etter forekomster i strenger eller bryte opp strenger i token, noe som gjør dem til et uunnværlig verktøy for effektiv og effektiv tekstbehandling.

## Hvordan:
C++11 introduserte støtte for regulære uttrykk i standardbiblioteket, `<regex>`, som tilbyr et robust rammeverk for strengsøking og manipulasjon. Her er et grunnleggende eksempel på bruk av regulære uttrykk for å søke etter et mønster i en streng:

```cpp
#include <iostream>
#include <regex>

int main() {
    std::string mål = "Hei, min e-post er eksempel@eksempel.com";
    std::regex epost_mønster(R"(\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}\b)");

    if (std::regex_search(mål, epost_mønster)) {
        std::cout << "E-post funnet!" << std::endl;
    } else {
        std::cout << "Ingen e-post funnet." << std::endl;
    }

    return 0;
}
```
**Eksempel på utskrift**
```
E-post funnet!
```

For mer komplekse manipulasjoner, som å erstatte mønstre i strenger, kan C++’s regulære uttrykk være veldig nyttige:

```cpp
#include <iostream>
#include <regex>

int main() {
    std::string tekst = "Regnet i Spania faller hovedsakelig på sletten.";
    std::regex vokal_regex("([aeiou])");

    std::string erstattet_tekst = std::regex_replace(tekst, vokal_regex, "*");
    std::cout << erstattet_tekst << std::endl;

    return 0;
}
```
**Eksempel på utskrift**
```
R*gn*t * Sp*n** f*ll*r h*v*ds*k*l*g p* sl*tt*n.
```

For programmerere som utforsker utover standardbiblioteket, er Boost Regex-biblioteket (`boost/regex.hpp`) et populært tredjepartsalternativ som tilbyr forbedrede regex-egenskaper og ytelsesoptimaliseringer, spesielt for komplekse mønstre eller omfattende databehandling:

```cpp
#include <iostream>
#include <boost/regex.hpp>

int main() {
    std::string s = "Boost-biblioteker er gøy!";
    boost::regex uttrykk("(\\w+)\\s(biblioteker)"); // Matcher "Boost-biblioteker"
    std::string fmt("GNU \\1"); // Erstatter med "GNU Boost"

    std::string resultat = boost::regex_replace(s, uttrykk, fmt);
    std::cout << resultat << std::endl;

    return 0;
}
```
**Eksempel på utskrift**
```
GNU Boost er gøy!
```

Disse eksemplene riper bare i overflaten av C++'s kapasiteter med regulære uttrykk, og illustrerer grunnleggende søk, mønstergjenkjenning og erstatninger, enten ved bruk av standardbiblioteket eller forbedret av Boosts kraftige regex-implementering.
