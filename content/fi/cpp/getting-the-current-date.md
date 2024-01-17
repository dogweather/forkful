---
title:                "Nykyisen päivämäärän hankkiminen"
html_title:           "C++: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Päivämäärän selvittäminen on yksinkertaisesti nykyisen päivämäärän hakemista. Tämä on hyödyllistä esimerkiksi sovelluksissa, joissa tarvitaan tietoa ajankohtaisista tapahtumista tai tietojen tallentamista päivämäärän mukaan.

## Miten tehdä:
```C++
#include <iostream>
#include <ctime>

int main() {
  // Selvitetään nykyinen päivämäärä aikaleiman avulla
  time_t now = time(0);

  // Muutetaan aikaleima muotoon local time
  char* dt = ctime(&now);

  // Tulostetaan nykyinen päivämäärä
  std::cout << "Nykyinen päivämäärä on: " << dt << std::endl;

  return 0;
}
```
Tuloste:
```
Nykyinen päivämäärä on: [Day of the Week] [Month] [Day] [Time] [Year]
```

## Syväsukellus:
Päivämäärän selvittäminen on ollut tärkeä osa ohjelmointia jo pitkään. Aikaisemmin päivämäärän selvittäminen vaati monimutkaisempia menetelmiä, kuten aikaleimojen laskemista ja kalenterin tietojen käyttöä. Nykyään lähes kaikki ohjelmointikielet tarjoavat sisäänrakennetun toiminnon päivämäärän hakemiseen.

Vaihtoehtona päivämäärän selvittämiselle on päivämääräkirjaston käyttö. Tämä voi olla hyödyllistä, jos tarvitaan tarkempaa ja monimutkaisempaa päivämääräkäsittelyä. Päivämäärän selvittämisessä on myös erilaisia vaihtoehtoja, kuten käyttäjän asettaman päivämäärän hakeminen tai päivämäärän hakeminen tietystä aikavyöhykkeestä.

## Katso myös:
- [std::time - C++ Reference](https://en.cppreference.com/w/cpp/chrono/c/time)
- [Boost.Date_Time - C++ Libraries](https://www.boost.org/doc/libs/1_75_0/doc/html/date_time.html)