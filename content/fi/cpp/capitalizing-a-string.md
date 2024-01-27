---
title:                "Merkkijonon muuttaminen isoiksi kirjaimiksi"
date:                  2024-01-19
html_title:           "Arduino: Merkkijonon muuttaminen isoiksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen isoiksi kirjaimiksi"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
"Mikä ja Miksi?"  
Tekstijonon kaikki merkit suuraakkosiksi muuttaminen on yksinkertaista: muutetaan kirjaimet A:sta Z:hen. Koodarit tekevät tätä yhtenäistämään tulosteita tai helpottaakseen vertailua, koska koneet erottavat isot ja pienet kirjaimet.

## How to:
"Näin toimit:"  
```cpp
#include <iostream>
#include <algorithm>
#include <string>

int main() {
    std::string viesti = "Ohjelmointi on hauskaa!";
    std::transform(viesti.begin(), viesti.end(), viesti.begin(), ::toupper);

    std::cout << viesti << std::endl; // OUTPUT: OHJELMOINTI ON HAUSKAA!
    return 0;
}
```

## Deep Dive
"Sukellus syvyyksiin"  
C++ ei suoraan tarjoa funktiota kokonaisen stringin suurentamiseen, mutta `<algorithm>`-kirjastossa on `transform`, joka sopii tähän käyttöön. Historiallisesti, tiedon käsittely on ollut kirjainkoosta riippumatonta, joten kielissä kuten C++ vaaditaan ylimääräisiä toimia tämän saavuttamiseksi. Vaihtoehtoina voi kirjoittaa oman toiminnallisuuden tai käyttää ulkopuolisia kirjastoja, kuten Boost.Algorithm-string-modifierit. Muutamassa yksityiskohdassa: `::toupper` on lokaalista riippumaton versio, mutta voi käyttää `std::toupper` tarpeen mukaan lokaalista riippuvaan muunnokseen.

## See Also
"Katso myös"  
- C++ Standard Library documentation: https://en.cppreference.com/w/
- Boost Algorithm Library: https://www.boost.org/doc/libs/release/libs/algorithm/
- C++ string handling tutorials: http://www.cplusplus.com/reference/string/string/
