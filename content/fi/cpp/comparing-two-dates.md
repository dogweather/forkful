---
title:    "C++: Kahden päivämäärän vertailu"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi vertailla kahta päivämäärää?

Päivämäärien vertailu on tärkeä osa ohjelmointia, sillä se mahdollistaa tiettyjen toimintojen suorittamisen tiettyinä päivinä tai aikaväleinä. Esimerkiksi verkkosivuilla saatetaan näyttää erilaisia tarjouksia tiettynä päivämääränä tai sovelluksessa saatetaan lähettää muistutus tärkeästä tapahtumasta tiettynä päivänä. Siksi on tärkeää tietää, miten kahta päivämäärää voidaan vertailla ja tarkistaa niiden välisiä eroja.

## Miten vertailla kahta päivämäärää?

Päivämäärien vertailemiseen on monia erilaisia tapoja, mutta yksi tehokkaimmista tavoista on käyttää valmiita aikaluokkia, kuten C++:n *tm* rakennetta tai *chrono* kirjastoa. Näillä työkaluilla on valmiita funktioita, jotka mahdollistavat päivämäärien vertailun helposti ja tarkasti.

```C++
#include <iostream>
#include <ctime>

using namespace std;

int main() {
    // Alustetaan kaksi päivämäärää
    tm date1 = { 0, 0, 0, 1, 3, 2020 }; // 1. maaliskuuta 2020
    tm date2 = { 0, 0, 0, 1, 6, 2020 }; // 1. kesäkuuta 2020

    // Muunnetaan päivämäärät sekunneiksi
    time_t time1 = mktime(&date1);
    time_t time2 = mktime(&date2);

    // Vertaillaan päivämääriä ja tulostetaan ero
    if (time1 < time2) {
        cout << "Ensimmäisen päivämäärän ja toisen päivämäärän ero on " 
            << difftime(time2, time1) / (24 * 60 * 60) << " päivää." << endl;
    } else {
        cout << "Toisen päivämäärän ja ensimmäisen päivämäärän ero on " 
            << difftime(time1, time2) / (24 * 60 * 60) << " päivää." << endl;
    }

    return 0;
}
```

*Esimerkkitulostus:*

```
Ensimmäisen päivämäärän ja toisen päivämäärän ero on 92 päivää.
```

## Syvempi sukellus päivämäärien vertailuun

Päivämäärien vertailussa tärkeintä on ymmärtää, mitä eri aikaluokat, kuten *tm* rakenne ja *chrono* kirjasto, tekevät taustalla. Nämä työkalut mahdollistavat päivämäärien käsittelyn eri muodoissa ja tarjoavat valmiita funktioita helpottamaan vertailua. On myös tärkeää huomata, että päivämäärien vertailuun vaikuttavat myös alueelliset asetukset, kuten aikavyöhykkeet, joten oikeiden tulosten saamiseksi on tärkeää olla tietoinen näistä asetuksista ja huolehtia niiden oikeasta määrittämisestä.

## Katso myös

- [C++ *tm* rakenne](https://en.cppreference.com/w/cpp/chrono/c/tm)
- [C++ *chrono* kirjasto](https://en.cppreference.com/w/cpp/chrono)
- [Päivämäärien vertailu eri aikavyöhykkeillä](https://www.learncpp.com/cpp-tutorial/using-time-and-clocks-in-c/)