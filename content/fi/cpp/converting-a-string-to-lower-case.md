---
title:                "Merkkijonon muuttaminen pienaakkosiksi"
html_title:           "C++: Merkkijonon muuttaminen pienaakkosiksi"
simple_title:         "Merkkijonon muuttaminen pienaakkosiksi"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Miksi koodaajat muuttavat merkkijonon pieniksi kirjaimiksi ja mitä se tarkalleen ottaen tarkoittaa? Merkkijonon muuntaminen pieniksi kirjaimiksi tarkoittaa, että kaikki merkit muutetaan pieniksi kirjaimiksi. Tämä on hyödyllistä esimerkiksi silloin, kun halutaan vertailla kahta merkkijonoa, mutta merkkijonossa on erilaisia kirjainkokoja.

## Kuinka?
Pieniksi muuttaminen on helppoa C++:ssa. Tässä on yksinkertainen esimerkki:
```C++
#include <iostream>
#include <algorithm>
#include <string>

int main() {
    std::string merkkijono = "TEKSTI";
    std::transform(merkkijono.begin(), merkkijono.end(), merkkijono.begin(), ::tolower);
    std::cout << merkkijono << "\n";
    // Output: teksti
    return 0;
}
```

## Syvällinen tarkastelu
Tämä toiminto on ollut käytössä jo pitkään koodaajien keskuudessa. Ennen C++:ia monet kielet, kuten C ja Pascal, eivät tukeneet merkkijonojen muuntamista suoraan. Siksi käytössä oli erityinen funktio, kuten esimerkiksi "tolower" C-kielessä. Toinen vaihtoehto voisi olla käyttää merkkijonojen vertailuun suoraan funktiota, joka sallii myös erilaiset kirjainkoot.

## Katso myös
https://en.cppreference.com/w/cpp/string/byte/tolower - tietoa "tolower" funktiosta ja sen käyttämisestä.