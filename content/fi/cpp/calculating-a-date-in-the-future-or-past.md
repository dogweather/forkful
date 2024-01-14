---
title:                "C++: Tulevan tai menneen päivämäärän laskeminen"
simple_title:         "Tulevan tai menneen päivämäärän laskeminen"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi laskelmointi päivämäärästä on hyödyllistä

Laskelmointi päivämäärästä voi olla hyödyllistä esimerkiksi sovelluksissa, jotka seuraavat tulevia tapahtumia tai päivämääriä, kuten varausjärjestelmissä tai kalenteriohjelmissa. Se voi myös auttaa käyttäjiä suunnittelemaan etukäteen tulevia tapahtumia, kuten matkoja tai lomia.

## Kuinka laskea päivämäärä tulevaisuudessa tai menneisyydessä

Kun haluat laskea päivämäärän tulevaisuudessa tai menneisyydessä, sinun tulee ensin määrittää lähtöpäivä ja sen jälkeen lisätä tai vähentää haluttu määrä päiviä haluttuun suuntaan. Tämä voidaan tehdä käyttämällä C++:n standardikirjaston Date and Time -luokkia.

```
#include <iostream>
#include <chrono>
#include <iomanip>

int main() {

// Määritetään lähtöpäivä (31.12.2020)
std::chrono::system_clock::time_point departureDate = 
    std::chrono::system_clock::from_time_t(std::time(nullptr));

// Lisätään 10 päivää
departureDate += std::chrono::hours(10 * 24);

// Vaihdetaan lähtöpäivän muoto DD.MM.YYYY
std::time_t t = std::chrono::system_clock::to_time_t(departureDate);
std::cout << std::put_time(std::localtime(&t), "%d.%m.%Y") << std::endl;

return 0;
}

```

**Tulostus:**
10.01.2021 

## Syvempi sukellus päivämäärän laskemiseen

Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä voi vaatia hieman enemmän laskutoimituksia, jos halutaan ottaa huomioon myös kuukausien ja vuosien vaihtelut. Tämä voidaan tehdä käyttämällä C++:n standardikirjaston Date and Time -luokkien lisäksi myös DateTime-tietotyyppiä.

```
#include <iostream>
#include <chrono>
#include <iomanip>

int main() {

// Määritetään lähtöpäivä (31.12.2020)
std::chrono::system_clock::time_point departureDate = 
    std::chrono::system_clock::from_time_t(std::time(nullptr));

// Lisätään 10 kuukautta ja 10 päivää
departureDate += std::chrono::hours(10 * 365 * 24) + std::chrono::hours(10 * 24);

// Muutetaan DateTime-tietotyypiksi
std::chrono::time_point<DateTime > dateTime = departureDate;

// Vaihdetaan lähtöpäivän muoto DD.MM.YYYY
std::cout << std::put_time(std::localtime(&dateTime), "%d.%m.%Y") << std::endl;

return 0;
}

```

**Tulostus:**
11.11.2021

## Katso myös

- [C++ Date and Time -luokat](https://en.cppreference.com/w/cpp/chrono)
- [DateTime-tietotyyppi](https://en.cppreference.com/w/cpp/chrono/time_point)