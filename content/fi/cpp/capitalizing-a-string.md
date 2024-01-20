---
title:                "Merkkijonon isoilla kirjaimilla"
html_title:           "C++: Merkkijonon isoilla kirjaimilla"
simple_title:         "Merkkijonon isoilla kirjaimilla"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Mitä & miksi?

Merkkijonojen muuntaminen isoiksi kirjaimiksi tarkoittaa, että jokainen merkkijonon kirjain muutetaan isoksi kirjaimeksi. Ohjelmoijat tekevät tämän yleisimmin tiedon käsittelyssä ja vertailussa, erityisesti silloin kun halutaan tehdä kirjainkoosta riippumattomia vertailuja.

## Kuinka:

```C++
#include <algorithm>
#include <cctype>
#include <iostream>
#include <string>

// Funktio joka muuntaa merkkijonon isoiksi kirjaimiksi
void muutaIsoiksiKirjaimiksi(std::string &s) {
    std::transform(s.begin(), s.end(), s.begin(), 
        [](unsigned char c){ return std::toupper(c); }
    );
}

int main() {
    std::string s = "tervetuloa c++ ohjelmointiin!";
    muutaIsoiksiKirjaimiksi(s);
    std::cout << s << std::endl;  // TULOS: TERVETULOA C++ OHJELMOINTIIN!
    return 0;
}
```

## Syvempi sukellus:

Historiallisesti merkkijonojen muuntaminen isoiksi kirjaimiksi on ollut yksinkertainen tapa tehdä vertailuja, jotka ovat herkkiä kirjainkoostumiselle. 

Kuten näemme, C++ tarjoaa erittäin tehokkaan ja yksinkertaisen tavan tehdä tämä `std::toupper` funktion avulla , joka on osa standardikirjastoa. Voimme myös käyttää `std::transform` funktiota läpikäymään merkkijonon ja soveltamaan `std::toupper` funktiota jokaiseen merkkiin.

Muut ohjelmointikielet saattavat tarjota myös muut metodit tai funktiot merkkijonojen käsittelyyn. Esimerkiksi Pythonissa löytyy sisäänrakennettu `upper` metodi.

## Katso myös:

- C++ Standard Library: http://www.cplusplus.com/reference/
- Transform function in C++ STL: https://www.geeksforgeeks.org/stdtransform-c-stl/?ref=lbp
- ASCII Table: https://ascii.cl/