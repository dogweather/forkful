---
title:                "Kahden päivämäärän vertaaminen"
html_title:           "C++: Kahden päivämäärän vertaaminen"
simple_title:         "Kahden päivämäärän vertaaminen"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Päivämäärien vertaaminen viittaa prosessiin, jossa kahta päivämäärää verrataan toisiinsa, esimerkiksi selvittämään, mikä niistä tulee ennen tai jälkeen tai ovatko ne samat. Ohjelmoijat tekevät tämän usein ajanjaksojen laskemiseksi tai aikataulutuksen hallitsemiseksi.

## Näin teet

```C++
#include <iostream>
#include <ctime>
#include <iomanip>

// esimerkki päivämäärien luomisesta ja vertaamisesta
int main() {
    // luodaan kaksi aikapiste-olioo
    std::tm a = {};
    std::tm b = {};

    a.tm_year = 120; a.tm_mon = 5; a.tm_mday = 25;   // 25.6.2020
    b.tm_year = 121; b.tm_mon = 5; b.tm_mday = 25;   // 25.6.2021

    // muunnetaan aikapisteet epoch-ajaksi (sekunneissa alkaen 1.1.1970)
    std::time_t x = std::mktime(&a);
    std::time_t y = std::mktime(&b);

    // verrataan sekunneiksi muutettuja aikoja
    if (x < y)
        std::cout << "a tulee ennen b:tä";
    else if (y < x)
        std::cout << "b tulee ennen a:ta";
    else
        std::cout << "a ja b ovat samana päivänä";
}

```

## Syvällisemmin

Historiallinen konteksti: Ajan laskeminen sekunneissa vuodesta 1970 eteenpäin, ns. "epoch-aika", on Unix-järjestelmien perintöä. Tämä muoto on hyvä päivämäärien vertailuun, koska se muuntaa päivämäärät yksinkertaisiksi numeroiksi.

Vaihtoehtoisia malleja ovat päivämäärä- ja aika-kirjastot, kuten [Date](https://en.cppreference.com/w/cpp/chrono) ja [Boost.Date_Time](https://www.boost.org/doc/libs/1_76_0/doc/html/date_time.html).

Yksityiskohtia toteutuksesta: C++ tarjoaa useita tapoja päivämäärien ja aikojen vertailemiseen. Tässä esimerkissä käytetty ```mktime``` -funktio muuntaa päivämäärän sekunneiksi, joiden vertailu on yksinkertaista.

## Katso myös

Hyödyllisiä lähteitä ovat:

1. C++ aikakirjaston virallinen dokumentaatio ([std::time](https://en.cppreference.com/w/cpp/chrono/c/tm))
2. Päivämäärien vertailun opas ([stackoverflow](https://stackoverflow.com/questions/6556700/comparing-date-in-c))
3. Lisätietoja epoch-ajasta ([Unix Time](https://en.wikipedia.org/wiki/Unix_time))