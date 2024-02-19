---
aliases:
- /nl/cpp/using-regular-expressions/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:19.603019-07:00
description: "Reguliere expressies zijn patronen die worden gebruikt om karaktercombinaties\
  \ in tekst te matchen. Programmeurs gebruiken ze voor taken zoals validatie,\u2026"
lastmod: 2024-02-18 23:09:02.171825
model: gpt-4-0125-preview
summary: "Reguliere expressies zijn patronen die worden gebruikt om karaktercombinaties\
  \ in tekst te matchen. Programmeurs gebruiken ze voor taken zoals validatie,\u2026"
title: Reguliere expressies gebruiken
---

{{< edit_this_page >}}

## Wat & Waarom?

Reguliere expressies zijn patronen die worden gebruikt om karaktercombinaties in tekst te matchen. Programmeurs gebruiken ze voor taken zoals validatie, zoeken en tekstmanipulatie vanwege hun kracht en flexibiliteit.

## Hoe te:

Om reguliere expressies in C++ te gebruiken, moet je de `<regex>` bibliotheek includeren. Hier is hoe je tekst matcht, zoekt en vervangt:

```cpp
#include <iostream>
#include <regex>

int main() {
    std::string target("Hallo Wereld. Dit is een regex test.");
    
    // Match
    std::regex match_pattern("Hallo Wereld");
    bool is_match = std::regex_match(target, match_pattern);
    std::cout << (is_match ? "Overeenkomst" : "Geen overeenkomst") << "\n";
    
    // Zoeken
    std::regex search_pattern("\\bis\\b");
    std::smatch matches;
    if (std::regex_search(target, matches, search_pattern)) {
        std::cout << "Gevonden: " << matches[0] << "\n";
    }

    // Vervangen
    std::regex replace_pattern("Wereld");
    std::string result = std::regex_replace(target, replace_pattern, "Universum");
    std::cout << "Na vervanging: " << result << "\n";
    
    return 0;
}
```

Voorbeelduitvoer:

```
Overeenkomst
Gevonden: is
Na vervanging: Hallo Universum. Dit is een regex test.
```

## Diepgaande Duik

Reguliere expressies zijn sinds de jaren 1950 een onderdeel van de informatica, gepopulariseerd door hulpprogramma's zoals grep in Unix. C++ heeft ze veel later aangenomen, met std::regex in C++11. Native ondersteuning varieert per compiler; sommige kunnen achterblijven in volledige regex-functieondersteuning.

Alternatieven voor `std::regex` omvatten bibliotheken zoals Boost.Regex of PCRE (Perl Compatible Regular Expressions). Boost.Regex presteert bijvoorbeeld vaak beter dan `std::regex` en heeft een rijker functieset.

Vanuit implementatie-oogpunt kan `std::regex` trager zijn dan sommige aangepaste parseralgoritmes, vooral voor eenvoudige patronen. Het begrijpen van de afweging tussen regex-gemak en potentiële prestatieproblemen is cruciaal.

## Zie Ook

- C++ referentie op `<regex>`: https://en.cppreference.com/w/cpp/regex
- Documentatie van Boost.Regex: https://www.boost.org/doc/libs/release/libs/regex/
- De officiële site van PCRE: https://www.pcre.org/

Verder lezen en tools om je regex-vaardigheden te verbeteren:

- Regular-Expressions.info Tutorial: https://www.regular-expressions.info/tutorial.html
- Regex101 (online tester): https://regex101.com/
