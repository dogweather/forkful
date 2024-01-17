---
title:                "Päivämäärän muuntaminen merkkijonoksi"
html_title:           "C++: Päivämäärän muuntaminen merkkijonoksi"
simple_title:         "Päivämäärän muuntaminen merkkijonoksi"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Mitä ja miksi?

Päivämäärän muuttaminen merkkijonoksi tarkoittaa, että ohjelmoija muokkaa päivämääräarvoa, esimerkiksi vuoden, kuukauden ja päivän numerosta, tekstiksi. Tämä on usein tarpeen, kun halutaan tulostaa päivämäärä käyttäjälle tai tallentaa se tiedostoon. Jonkin päivämäärän muuttaminen merkkijonoksi helpottaa sen käsittelyä ohjelmassa.

# Kuinka:

```cpp
#include <iostream>
#include <string>
#include <sstream>

using namespace std;

int main() {

    // Alustetaan päivä, kuukausi ja vuosi muuttujat
    int day = 15;
    int month = 9;
    int year = 2021;

    // Luodaan stringstream-olio, johon päivämäärä muutetaan
    stringstream ss;

    // Lisätään päivämääräarvot stringstreamiin
    ss << day << "." << month << "." << year;

    // Muutetaan stringstreamin sisältö merkkijonoksi
    string date = ss.str();

    // Tulostetaan päivämäärä merkkijonona
    cout << "Tänään on " << date << endl;

    // Output: Tänään on 15.9.2021

    return 0;
}
```

# Syväsukellus:

Päivämäärän muuttaminen merkkijonoksi on ollut tarpeellista jo pitkään ohjelmoinnin historiassa. Ennen C++:aa tämä oli mahdollista tehdä vain käsittelemällä merkkijonoja C-kielellä. Nykyään C++ tarjoaa monia valmiita toimintoja, kuten stringstreamin, jotka helpottavat tämän tehtävän hoitamista. On myös mahdollista muuttaa päivämäärä suoraan merkkijonoksi esimerkiksi käyttämällä kirjastoa, kuten Boost.Date_Time.

# Katso myös:

- https://www.cplusplus.com/reference/sstream/stringstream/ - C++ referenssisivu stringstream-luokasta
- https://www.boost.org/doc/libs/1_77_0/doc/html/date_time.html - Boost.Date_Time kirjaston dokumentaatio