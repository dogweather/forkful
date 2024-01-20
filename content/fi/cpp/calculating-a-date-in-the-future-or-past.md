---
title:                "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
html_title:           "C++: Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
simple_title:         "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen C++-ohjelmoinnissa

## Mikä & Miksi?

Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen on sen määrittämistä, mitä päivämäärää edustaa tietty ajanjakso ennen tai jälkeen tiedetyn päivämäärän. Ohjelmoijat tekevät tämän usein aikavalintojen hallintaan tai ajanjaksojen laskemiseen sovelluksissa.

## Näin teet:

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    // Nykyinen aikaUTC
    auto t_nyt = std::chrono::system_clock::now();

    // Lisätään 30 päivää
    auto tulevaisuus = t_nyt + std::chrono::hours(24 * 30);
    std::time_t t = std::chrono::system_clock::to_time_t(tulevaisuus);

    std::cout << "30 päivän päästä on: " << ctime(&t) << '\n';

    // Otetaan pois 20 päivää
    auto menneisyys = t_nyt - std::chrono::hours(24 * 20);
    t = std::chrono::system_clock::to_time_t(menneisyys);
    
    std::cout << "20 päivää sitten oli: " << ctime(&t) << '\n';

    return 0;
}
```

## Syväluotaus

Päivämäärän laskeminen tulevaisuuteen tai taaksepäin ei ole uusi konsepti. Se ulottuu kauas päivämäärien ja kellonaikojen historian alkuun. Ajan myötä tämä operaatio on muuttunut järjestelmästä ja tekniikasta toiseen. C++:ssa on useita vaihtoehtoja, kuten vanhempi `ctime`-kirjasto tai uudempi `chrono`, jota käytettiin näytteenä.

`std::chrono` on moderni tapa käsitellä aikaan liittyviä laskutoimituksia C++:ssa. Se otettiin käyttöön C++11-standardissa ja sisältää tyypit sekuntien, millisekuntien jne. käsittelyyn. `system_clock` on yleisin kello, joka on kytketty järjestelmän hetkelliseen aikaan.

Voit käytää `duration_cast` funktiota jos haluat tarkempia ajanjaksoja esimerkiksi millisekuntitasolla, ja voit yhdistellä sen `chrono_literals`-nimisen nimiavaruuden kanssa lisäämään koodiisi aikakonstantteja, kuten `24h` tai `30d`. 

## Katso myös:

1. C++ chrono-kirjaston dokumentointi: [cppreference.com/w/cpp/chrono](https://cppreference.com/w/cpp/chrono)
2. Ajan käsittely C++:ssa videoesittely: [youtube.com/watch?v=uf1zJyk36P8](https://www.youtube.com/watch?v=uf1zJyk36P8)
3. Päivämäärän laskeminen C++:ssa StackOverflow: [stackoverflow.com/questions/1068827/date-calculation](https://stackoverflow.com/questions/1068827/date-calculation)