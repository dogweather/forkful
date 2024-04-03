---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:15.392420-07:00
description: "Hvordan: C++11 introduserte st\xF8tte for regul\xE6re uttrykk i standardbiblioteket,\
  \ `<regex>`, som tilbyr et robust rammeverk for strengs\xF8king og manipulasjon.\u2026"
lastmod: '2024-03-13T22:44:41.089330-06:00'
model: gpt-4-0125-preview
summary: "C++11 introduserte st\xF8tte for regul\xE6re uttrykk i standardbiblioteket,\
  \ `<regex>`, som tilbyr et robust rammeverk for strengs\xF8king og manipulasjon."
title: "Bruke regul\xE6re uttrykk"
weight: 11
---

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
