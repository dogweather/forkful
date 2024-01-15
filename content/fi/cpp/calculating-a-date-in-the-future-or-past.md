---
title:                "Laskeminen tulevaan tai menneeseen päivämäärään"
html_title:           "C++: Laskeminen tulevaan tai menneeseen päivämäärään"
simple_title:         "Laskeminen tulevaan tai menneeseen päivämäärään"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Olet varmasti joutunut tilanteeseen, jossa sinun täytyy laskea päivämäärä tulevaisuudessa tai menneisyydessä. Ehkä suunnittelet tapahtumaa tai haluat tietää tarkalleen milloin tiettyä tapahtumaa tapahtuu. C++:lla voit helposti laskea haluamasi päivämäärän tulevaisuudessa tai menneisyydessä käyttäen muutamia käteviä funktioita, jotka esittelemme tässä artikkelissa.

## Miten

Laskettaessa päivämäärää tulevaisuudessa tai menneisyydessä, on tärkeää ymmärtää, miten päivämäärä tallennetaan tietokoneelle. C++:ssa päivämäärä tallennetaan yleensä kokonaislukuna, jossa käytetään tiettyä formaattia. Tämän kokonaisluvun avulla voimme käyttää erilaisia funktioita, jotka auttavat meitä laskemaan haluamamme päivämäärän.

Esimerkiksi voimme käyttää `time.h` kirjastoa ja sen `time_t` tietotyyppiä laskeaksemme tämän päivän päivämäärän sekunteina tammikuun 1. 1970:stä lähtien. Tämän jälkeen voimme lisätä tai vähentää haluamamme määrän sekunteja saadaksemme tulevaisuuden tai menneisyyden päivämäärän.

Esimerkiksi, jos haluamme laskea päivämäärän 10 päivää tulevaisuudessa, voimme käyttää seuraavaa koodia:

```C++
time_t t = time(NULL);
t += (10*24*60*60); // 10 päivää * 24 tuntia * 60 minuuttia * 60 sekuntia
```

Tämän jälkeen voimme käyttää `localtime` funktiota muuntamaan `time_t` päivämääräksi ja tulostaa sen haluamassamme formaatissa. Esimerkiksi:

```C++
struct tm *p = localtime(&t);
cout << "Päivämäärä 10 päivän päästä: " << p->tm_mday << "." << p->tm_mon + 1 << "." << p->tm_year + 1900 << endl;
```

Tämä tulostaisi esimerkiksi "Päivämäärä 10 päivän päästä: 19.8.2020" (olettaen, että tämän artikkelin kirjoitushetkellä on elokuu 2020).

## Syventyvä tieto

Kuten edellä mainittiin, päivämäärän tallennuksessa tietokoneelle käytetään yleensä tiettyä formaattia. Tämän formaatin ymmärtäminen auttaa välttämään virheitä laskiessa päivämäärää tulevaisuudessa tai menneisyydessä.

Yleisin päivämäärän tallennusformaatti on POSIX-aikaleima, joka esittää päivämäärän sekunteina tammikuun 1. 1970:stä lähtien. Tämä formaatti on kätevä, sillä se tekee päivämäärän laskemisesta helpompaa. Tiettyjen muotoilujen tulostamiseksi voidaan käyttää `strftime` funktiota.

On myös huomattava, että erilaisissa aikavyöhykkeissä käytetään erilaisia päivämäärän alkuarvoja. Esimerkiksi Yhdysvalloissa käytetään tammikuun 1. 1601 päivämääränä, kun taas Japanissa käytetään tammikuun 1.