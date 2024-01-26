---
title:                "Merkkijonosta päivämäärän jäsentäminen"
date:                  2024-01-20T15:34:54.167632-07:00
html_title:           "Bash: Merkkijonosta päivämäärän jäsentäminen"
simple_title:         "Merkkijonosta päivämäärän jäsentäminen"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Päivämäärän jäsentäminen merkkijonosta tarkoittaa päivämäärän erottamista tekstistä ja sen muuntamista hyödynnettävään muotoon. Ohjelmoijat tekevät tätä syöttöjen käsittelyn ja päivämäärätietojen hyödyntämisen helpottamiseksi.

## Kuinka:
```C++
#include <iostream>
#include <string>
#include <sstream>
#include <iomanip>
#include <chrono>

int main() {
    std::string date_str = "2023-04-05";
    std::istringstream ss(date_str);
    std::tm dt = {};
    
    ss >> std::get_time(&dt, "%Y-%m-%d");
    if(ss.fail()) {
        std::cout << "Virheellinen päivämäärä!" << std::endl;
        return 1;
    }

    std::cout << "Vuosi: " << dt.tm_year + 1900 << std::endl; // tm_year on vuosia vuodesta 1900
    std::cout << "Kuukausi: " << dt.tm_mon + 1 << std::endl;  // tm_mon kuukausia alkaen 0
    std::cout << "Päivä: " << dt.tm_mday << std::endl;         // tm_mday päiviä alkaen 1
    return 0;
}
```
Tuloste:
```
Vuosi: 2023
Kuukausi: 4
Päivä: 5
```

## Syväsukellus:
Päivämäärän jäsentämisen tarve juontaa juurensa aikaan, jolloin tietojenkäsittely alkoi digitalisoitua. Historiallisesti päivämäärät tallennettiin ja prosessoitiin monin eri tavoin. Standardi kirjastojen, kuten `<chrono>` ja `<iomanip>`, käyttö parantaa luettavuutta ja vähentää virheiden riskiä.

Vaihtoehtona voi käyttää `std::chrono` kirjastoa, joka tarjoaa lisää työkaluja ajan käsittelyyn C++20 standardista alkaen. Kokonaisuudessaan päivämäärän jäsentäminen voi olla monimutkaista eri formaattien ja aikavyöhykkeiden takia, joten standardikirjaston toiminnot tarjoavat hyvän pohjan monipuoliselle käsittelylle.

## Katso Myös:
- C++ `<chrono>` dokumentaatio: https://en.cppreference.com/w/cpp/chrono
- C++ `<iomanip>` dokumentaatio: https://en.cppreference.com/w/cpp/io/manip/get_time
- ISO 8601 Päivämäärä- ja aikaformaatit: https://en.wikipedia.org/wiki/ISO_8601
