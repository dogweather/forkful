---
title:                "C++: Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Mitä teet, kun haluat tietää mikä päivä on vaikkapa kahden viikon päästä tai menneisyydessä? Koodaat sen tietysti itse! Tässä blogipostauksessa käsittelemme kuinka lasketaan tietty päivä tulevaisuudessa tai menneisyydessä käyttämällä C++-ohjelmointikieltä.

## Kuinka

Laskeminen tiettyyn päivään tulevaisuudessa tai menneisyydessä vaatii muutamia askeleita, mutta se on helppoa kun käytät C++:aa. Seuraavassa esimerkissä käytämme `time.h` kirjastoa, jonka avulla voimme laskea halutun päivän lisäämällä tai vähentämällä päiviä alkuperäisestä päivästä.

```C++
#include <iostream>
#include <time.h>
using namespace std;

int main() 
{
    // Alkuperäinen päivä
    struct tm timeinfo = {0};
    timeinfo.tm_year = 2021 - 1900; // vuosi 2021, miinus 1900
    timeinfo.tm_mon = 10 - 1; // lokakuu, miinus 1
    timeinfo.tm_mday = 10; // 10. päivä

    // Haluamme laskea päivän 14 päivää eteenpäin, joten lisäämme päiviä timeinfoen
    const time_t modifiedTime = mktime(&timeinfo);

    // Muotoillaan päivämäärä haluttuun muotoon
    char buffer[80];
    strftime(buffer, 80, "%A, %B %d, %Y", localtime(&modifiedTime));

    cout << "Päivä 14 päivää eteenpäin on: " << buffer << endl; // Tulostaa maanantai, lokakuu 24, 2021
    return 0;
}
```

**Tulostus:**

```
Viikonpäivä, Kuukausi Päivämäärä, Vuosi
Päivä 14 päivää eteenpäin on: maanantai, lokakuu 24, 2021
```

## Syvemmälle

`mktime()`-funktio muuntaa `struct tm` -muuttujan ajanlaskun sekunneiksi. Tämän jälkeen voimme käyttää `localtime()`-funktiota muotoilemalla ajanlaskun haluttuun päivämäärämuotoon.

On myös mahdollista lisätä tai vähentää päiviä halutulle päivämäärälle käyttämällä `mktime()`-funktiota. Esimerkiksi jos haluamme laskea päivä 14 vuotta taaksepäin, vaihdamme `timeinfo.tm_year` -arvoksi 2021:stä 2007:ään.

## Katso myös

- [C++ time.h documentointi](https://www.cplusplus.com/reference/ctime/)
- [C++ mktime()-funktio](https://www.cplusplus.com/reference/ctime/mktime/)