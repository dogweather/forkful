---
title:                "Päivämäärän muuttaminen merkkijonoksi"
html_title:           "Go: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Muunnetaan päivämäärä merkkijonoksi - prosessi, jossa ajankohta esitetään teksti muodossa. Ohjelmoijat tekevät tämän tiedonlukemisen helpottamiseksi ja tiedon käsittelyn yksinkertaistamiseksi.

## Kuinka tehdä:
Käytämme C++ Standard Kirjastosta päivämäärä- ja aikaluokkia tämän toiminnallisuuden toteuttamiseen.

```C++
#include <iostream>
#include <chrono>
#include <iomanip>

int main() {
    auto nykyinen_aika = std::chrono::system_clock::now();
    std::time_t muotoiltu_aika = std::chrono::system_clock::to_time_t(nykyinen_aika);

    std::cout << "Päivämäärä merkkijonona: " << std::put_time(std::localtime(&muotoiltu_aika), "%Y-%m-%d %H:%M:%S") << '\n';
}
```
Esim. tulostus saattaa näyttää tältä: `Päivämäärä merkkijonona: 2023-08-04 12:13:14`  

## Syvempi sukellus
Historiallisesti päivämäärät ja ajat esitettiin merkkijonoina muotoiluvaihtoehtojen erilaisuuden vuoksi. Tässä koodissa käytetty `std::put_time` -funktio ja formatter `%Y-%m-%d %H:%M:%S` ovat ISO 8601-standardin mukaisia. 

Vaihtoehtoisia metodeja päivämäärän merkkijonoksi muuntamiseen on olemassa, esimerkiksi toisten kirjastojen avulla, kuten Boost, mutta varsinkin vanhemmissa ohjelmissa kustomoidut funktiot ovat yleisiä.

`std::chrono::system_clock::now()` ottaa nykyisen ajan ja `std::chrono::system_clock::to_time_t` muuntaa sen ajaksi, joka on yhteensopiva C:n aikafunktioiden kanssa. Tämä aika muunnetaan paikalliseksi `std::localtime`-funktiolla, joka tuottaa yhteensopivan osoittimen `std::put_time`-funktiolle.

## Katso myös
1. [C++ Standard Library: <chrono>](http://www.cplusplus.com/reference/chrono/)
2. [C++ Standard Library: <iomanip>](http://www.cplusplus.com/reference/iomanip/)
3. [C++ Date and Time - Cplusplus.com](http://www.cplusplus.com/doc/tutorial/ntcs/)
5. [C++ Boost Date_Time library](https://www.boost.org/doc/libs/1_77_0/doc/html/date_time.html)