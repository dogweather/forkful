---
title:                "Substrings extraheren"
date:                  2024-01-28T21:59:44.012006-07:00
model:                 gpt-4-0125-preview
simple_title:         "Substrings extraheren"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/cpp/extracting-substrings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het extraheren van substrings betekent het wegnemen van kleine stukjes uit een grotere string. Programmeurs doen dit om specifieke data binnen een tekst te isoleren, te verwerken of te analyseren, zoals het extraheren van gebruikersnamen uit e-mailadressen of datums uit logboeken.

## Hoe te:

C++ maakt het gemakkelijk om een substring te pakken. `std::string` is hier onze betrouwbare hulp, met de functie `substr()` die het meeste zware werk doet. Laten we direct naar de code gaan:

```C++
#include <iostream>
#include <string>

int main() {
    std::string volledigeString = "Hallo, Wereld! Programmeren in C++ is leuk.";
    std::string fragment;

    // Extraheer "Wereld" startend bij index 7 met lengte 5
    fragment = volledigeString.substr(7, 5);
    std::cout << fragment << std::endl; // Uitvoer: Wereld

    // Extraheer "Programmeren" startend bij index 14
    fragment = volledigeString.substr(14);
    std::cout << fragment << std::endl; // Uitvoer: Programmeren in C++ is leuk.

    return 0;
}
```

## Diepere Duik

Substrings zijn niet nieuw. Oude C-programmeurs gebruikten `strncpy` en handmatige administratie. Het behandelen van strings is een veelvoorkomende bron van bugs, dus C++ wilde dit vereenvoudigen. `std::string` en zijn `substr` methode dateren uit C++98 en hebben sindsdien voor verlichting gezorgd.

Alternatieven? Zeker. Je zou handmatig kunnen gaan met `std::string::iterator` of oude C-functies afstoffen—als je graag gevaarlijk leeft. Een modernere aanpak zou string_views kunnen betreffen voor niet-wijzigende inkijkjes.

Implementatie? Onder de motorkap, `substr` wijst vaak nieuwe opslag toe en kopieert data, wat niet gratis is. Het is lichter in vergelijking met worstelen met ruwe pointers en char arrays van de oude tijden, maar het is niet instant.

## Zie Ook

Voor meer over `std::string` en zijn vrienden:
- cppreference.com over `std::string`: https://en.cppreference.com/w/cpp/string/basic_string
- Meer over `std::string_view`: https://en.cppreference.com/w/cpp/string/basic_string_view
- C-stijl string behandeling (voor de historische kicks): http://www.cplusplus.com/reference/cstring/
