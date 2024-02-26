---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:20.377403-07:00
description: "Het verwijderen van aanhalingstekens uit een string betekent het wegpeuteren\
  \ van die vervelende dubbele of enkele karakters die onze tekst omsluiten (' of\u2026"
lastmod: '2024-02-25T18:49:48.434344-07:00'
model: gpt-4-0125-preview
summary: "Het verwijderen van aanhalingstekens uit een string betekent het wegpeuteren\
  \ van die vervelende dubbele of enkele karakters die onze tekst omsluiten (' of\u2026"
title: Quotes verwijderen uit een string
---

{{< edit_this_page >}}

## Wat & Waarom?
Het verwijderen van aanhalingstekens uit een string betekent het wegpeuteren van die vervelende dubbele of enkele karakters die onze tekst omsluiten (' of "). Programmeurs doen dit vaak om invoer te zuiveren, tekst op te slaan in een database, of strings voor te bereiden voor verdere verwerking zonder de rommel van aanhalingstekens.

## Hoe:
Hier is een eenvoudige manier om die aanhalingstekens in C++ opzij te zetten:

```cpp
#include <iostream>
#include <algorithm>

std::string remove_quotes(std::string input) {
    input.erase(std::remove(input.begin(), input.end(), '\"'), input.end());
    input.erase(std::remove(input.begin(), input.end(), '\''), input.end());
    return input;
}

int main() {
    std::string original = R"("Hallo, 'Wereld'!")";
    std::string no_quotes = remove_quotes(original);
    std::cout << no_quotes << std::endl;
    return 0;
}
```

Voer dit uit, en je zult krijgen:

```
Hallo, Wereld!
```

Voilà! De aanhalingstekens zijn verdwenen.

## Diepere Duik
Aanhalingstekens zijn al sinds de dageraad van de computer een tekstergernis. Vroeger zag je programmeurs moeizaam door elk karakter lopen om die aanhalingstekens te filteren. Tegenwoordig hebben we `std::remove` in de Standard Template Library (STL) om het zware werk te doen.

Alternatieven? Zeker! Je zou reguliere expressies met `std::regex` kunnen gebruiken om aanhalingstekens te richten, maar dat is een beetje alsof je een mokerslag gebruikt om een noot te kraken - krachtig, maar kan overkill zijn voor eenvoudige taken. Voor degenen die de recente C++ smaken prefereren, kun je experimenteren met `std::string_view` voor benaderingen zonder aanpassingen.

Wat de implementatie betreft, onthoud dat `std::remove` daadwerkelijk geen elementen uit de container verwijdert; het schuift niet-verwijderde elementen naar voren en retourneert een iterator voorbij het nieuwe einde van het bereik. Daarom is de `erase` methode nodig om de ongewenste staart af te hakken.

## Zie Ook
- C++ `std::remove` referentie: [cppreference.com](https://en.cppreference.com/w/cpp/algorithm/remove)
- Meer over `std::string` manipulatie: [cplusplus.com](http://www.cplusplus.com/reference/string/string/)
