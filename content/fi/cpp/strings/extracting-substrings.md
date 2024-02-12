---
title:                "Merkkijonojen osien poimiminen"
aliases:
- /fi/cpp/extracting-substrings/
date:                  2024-01-20T17:45:06.144602-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonojen osien poimiminen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? - Mitä & Miksi?
Haemme tekstin osia, eli substringejä, kun tarvitsemme erityistä tietoa merkkijonosta. Ohjelmoijat tekevät niin, jotta voivat käsitellä, verrata tai analysoida tekstin osia.

## How to - Miten tehdään
```C++
#include <iostream>
#include <string>

int main() {
    std::string fullText = "C++ on kiva ohjelmointikieli!";
    std::string subText = fullText.substr(5, 2); // ottaa "on"

    std::cout << "Substring: " << subText << std::endl; // Tulostaa: Substring: on
    
    return 0;
}
```
## Deep Dive - Syväsukellus
Substringit ovat osa C++:n standardikirjastoa `std::string` luokan sisällä. Historiallisesti, C:n kielestä periytyvä C++ toi monia string-käsittelyn funktioita, mutta C++ ehdotti `std::string` ja metodeja, kuten `substr`, tarjoten objektiivisen otteen tekstinkäsittelyyn. Vaihtoehtoisesti voit käyttää C-tyylistä merkkijonojen käsittelyä funktioilla kuten `strncpy`, mutta siirtymät `std::string`-luokan käyttöön tuovat vähemmän virheriskejä ja käytön helppoutta. `substr`-metodi luo uuden string-olion alkuperäisen merkkijonon osasta, alkaen annetusta indeksistä ja annetun pituuden verran. Jos pituus jätetään antamatta, loppu merkkijono kopioidaan.

## See Also - Katso Myös
- C++ Standard Library reference: https://en.cppreference.com/w/cpp/string/basic_string/substr
- C++ strings tutorial: https://www.cplusplus.com/reference/string/string/
