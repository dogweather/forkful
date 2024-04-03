---
date: 2024-01-20 17:45:06.144602-07:00
description: "Haemme tekstin osia, eli substringej\xE4, kun tarvitsemme erityist\xE4\
  \ tietoa merkkijonosta. Ohjelmoijat tekev\xE4t niin, jotta voivat k\xE4sitell\xE4\
  , verrata tai\u2026"
lastmod: '2024-03-13T22:44:56.854111-06:00'
model: gpt-4-1106-preview
summary: "Haemme tekstin osia, eli substringej\xE4, kun tarvitsemme erityist\xE4 tietoa\
  \ merkkijonosta."
title: Merkkijonojen osien poimiminen
weight: 6
---

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
