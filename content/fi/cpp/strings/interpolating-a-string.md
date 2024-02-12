---
title:                "Merkkijonon interpolointi"
aliases: - /fi/cpp/interpolating-a-string.md
date:                  2024-01-20T17:50:36.726573-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonon interpolointi"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Merkkijonon interpolointi tarkoittaa muuttujien ja lausekkeiden yhdistämistä merkkijonojen sisään. Sitä käytetään dynaamisen tekstin luontiin, joka helpottaa esimerkiksi käyttäjän syötteiden tai muuttujien arvojen näyttämistä ohjelmassa.

## Miten:
```C++
#include <iostream>
#include <string>

int main() {
    int age = 25;
    std::string name = "Jarkko";
    // C++20 std::format toimintoa hyödyntäen
    std::string greeting = std::format("Hei! Nimeni on {0} ja olen {1} vuotta vanha.", name, age);
    std::cout << greeting << std::endl;
    return 0;
}
```
Tuloste tulisi olla:
```
Hei! Nimeni on Jarkko ja olen 25 vuotta vanha.
```

## Syväsukellus
Merkkijonon interpolointi on ollut ohjelmoinnissa käytössä jo vuosikymmeniä. Esimerkiksi C:ssä yhdisteltiin tekstejä printf-funktion avulla, mutta C++ otti askeleen eteenpäin tarjoten paremman typeturvallisuuden ja helpomman syntaksin `std::format`-funktion myötä C++20:ssä. Aiemmin saatettiin käyttää `std::ostringstream`-luokkaa tai raskaita `sprintf`-funktioita, mutta `std::format` tekee saman tehokkaammin ja ilman buffer overflow -riskejä. 

Vaihtoehtoisia menetelmiä ovat esimerkiksi string stream -luokat (`std::stringstream`, `std::ostringstream`) ja `boost::format` kirjaston käyttö, jos C++20 ei ole käytössä.

## Katso Myös
- C++20 `std::format`: https://en.cppreference.com/w/cpp/utility/format/format
- Historiaa `printf`-funktiosta: https://en.wikipedia.org/wiki/Printf_format_string
- `std::ostringstream`: https://en.cppreference.com/w/cpp/io/basic_ostringstream
