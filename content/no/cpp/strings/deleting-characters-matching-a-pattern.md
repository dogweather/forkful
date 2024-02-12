---
title:                "Slette tegn som matcher et mønster"
aliases: - /no/cpp/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:41:41.348490-07:00
model:                 gpt-4-1106-preview
simple_title:         "Slette tegn som matcher et mønster"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Slette tegn som matcher et mønster betyr å fjerne spesifikke karakterer fra en streng som oppfyller gitte kriterier. Programmerere gjør dette for å rense input, manipulere tekst, eller forberede data for videre behandling.

## Hvordan:
```C++
#include <iostream>
#include <regex>
#include <string>

int main() {
    std::string tekst = "Hallo, Verden! 123.";
    std::regex moenster("[^A-Za-z ]"); // Definerer et mønster for å slette ikke-bokstaver.
    std::string rensetTekst = std::regex_replace(tekst, moenster, "");

    std::cout << rensetTekst << std::endl; // Skriver ut den rensede teksten.
    return 0;
}
```

Sample output:
```
Hallo Verden
```

## Dypdykk
Fjerne tegn som matcher et mønster – det er ikke nytt. Regex, eller regulære uttrykk, ble introdusert på 1950-tallet. Nå brukes det i mange programmeringsspråk for tekstmanipulering.

Alternativer til regex inkluderer manuelle løkker for karakter-inspeksjon og erstatning, string-funksjoner som `find()` og `erase()`, eller tredjeparts biblioteker som Boost. Ved implementering er det viktig å merke seg at bruk av regex kan være tungt ressursmessig, så vurder enkel string-manipulasjon hvis ytelse er kritisk.

## Se Også
- C++ Reference for regex: https://en.cppreference.com/w/cpp/regex
- Boost Library Documentation: https://www.boost.org/doc/libs/release/libs/regex/
- "Mastering Regular Expressions" bok for dyptgående forståelse: http://shop.oreilly.com/product/9780596528126.do
