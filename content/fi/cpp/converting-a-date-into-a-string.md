---
title:    "C++: Päiväyksen muuntaminen merkkijonoksi"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmointiprojekteissa saattaa olla tarve muuttaa päivämäärä muotoon teksti. Tällainen muunnos voi olla hyödyllinen esimerkiksi, kun halutaan tulostaa päivämäärä tietokannasta löytyivän tiedon yhteydessä tai tallentaa päivämäärä tekstimuodossa tiedostoon.

## Kuinka

Seuraavassa on esimerkki C++-koodista, joka muuttaa nykyisen päivämäärän muotoon "päivä.kuukausi.vuosi":

```C++
#include <iostream>
#include <string>
#include <ctime>

using namespace std;

int main()
{
    // Haetaan nykyinen aika ja tallennetaan se aika-muuttujaan
    time_t now = time(0);

    // Muutetaan aika aikarakenteeksi
    tm *local_time = localtime(&now);

    // Luodaan merkkijono päivämäärälle
    string date = to_string(local_time->tm_mday) + "." + to_string(local_time->tm_mon + 1) + "." + to_string(local_time->tm_year + 1900);

    // Tulostetaan päivämäärä konsoliin
    cout << "Nykyinen päivämäärä: " << date << endl;

    return 0;
}
```

Tässä esimerkissä käytetään C++:n <ctime> ja <string> kirjastoja sekä aikarakennetta tm. Funktio localtime() palauttaa nykyisen ajan ja tämänhetkisen päivämäärän tiedot. Tämän jälkeen käytetään to_string()-funktiota muuttamaan numerot merkkijonoiksi ja lopuksi ne yhdistetään pisteillä erotettuun päivämäärämuotoon. Koodin tuloste näyttää esimerkiksi seuraavalta:

```
Nykyinen päivämäärä: 8.1.2021
```

## Syväsukellus

Käytettäessä <ctime> kirjastoa, on tärkeää huomata että tm-rakenne sisältää päivämäärän tiedot vuosittain + 1900 ja kuukaudet ovat numeroina 0-11 välillä. Lisäksi tm-rakenteessa on myös muita tietoja, kuten tunnit, minuutit ja sekunnit, joiden avulla voidaan muodostaa myös kellonaika.

On myös hyvä huomioida, että tämä esimerkki käyttää tietokoneen paikallista aikavyöhykettä päivämäärän muuttamiseen. Jos halutaan esimerkiksi muuttaa päivämäärä toisen aikavyöhykkeen mukaan, täytyy käyttää hieman erilaista lähestymistapaa.

## Katso myös

- C++ <ctime>, <string> kirjastot https://www.cplusplus.com/reference/ctime/
- C++ to_string() funktio https://www.cplusplus.com/reference/string/to_string/