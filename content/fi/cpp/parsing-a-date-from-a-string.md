---
title:                "Päivämäärän jäsentäminen merkkijonosta"
html_title:           "C++: Päivämäärän jäsentäminen merkkijonosta"
simple_title:         "Päivämäärän jäsentäminen merkkijonosta"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Päivämäärän parsiminen merkkijonosta on keino muuttaa merkkijono päiväysmetodiksi, jotta sitä voi käyttää laskutoimituksissa tai tarkastuksissa. Ohjelmoijat käyttävät tätä työkalua helpottaakseen päivämäärien käsittelyä ohjelmoinnissa.

## Miten tehdään:
### Esimerkki 1:
```C++
#include <iostream>
#include <string>
#include <ctime>

int main() {
  // Alustetaan merkkijono, josta halutaan parsia päiväys
  std::string merkkijono = "18.09.2021";
  // Alustetaan rakenteet päivämäärälle, johon parsitaan päiväys
  std::tm aika = {};
  // Parsitaan merkkijonosta päivämäärä
  std::istringstream ss(merkkijono);
  ss >> std::get_time(&aika, "%d.%m.%Y");

  std::cout << std::put_time(&aika, "Päivämäärä: %x") << std::endl;

  return 0;
}
```
### Tuloste:
```Päivämäärä: 09/18/2021```

## Syväkurkkaus:
Parsing (parsiminen) on termi, jota käytetään tietojenkäsittelyssä tarkoittamaan merkkijonon analysointia ja sen muuttamista tietorakenteiksi. Päivämäärän parsiminen merkkijonosta on tärkeää, koska päivämäärät ovat yleisiä tietoja, joita tarvitaan usein ohjelmoinnissa. Toinen tapa parsia päivämäärämerkkijono on käyttää pvm_atok (time_P) -funktiota, joka toimii vastakkaisessa suunnassa kuin get_time().

## Katso myös:
- https://en.cppreference.com/w/cpp/io/manip/get_time
- https://en.cppreference.com/w/cpp/chrono/c/strftime