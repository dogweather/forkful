---
title:    "C++: Lasketaan päivämäärä tulevaisuudessa tai menneisyydessä"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittaessasi ohjelmaa, joka käsittelee ajankohtia, saattaa tulla tarve laskea päivämääriä tulevaisuuteen tai menneisyyteen. Tämä voi olla hyödyllistä esimerkiksi kun haluat ohjelman näyttämään tietyn päivän tai laskemaan tietyn ajanjakson päästä.

## Kuinka tehdä

```C++
#include <iostream>
#include <ctime>

using namespace std;

int main()
{
   // Määritellään tänään oleva päivämäärä
   time_t t = time(0);
   struct tm * now = localtime( &t );

   // Tulostetaan päivämäärä
   cout << "Tänään on " << (now->tm_year + 1900) << '-' 
        << (now->tm_mon + 1) << '-'
        << now->tm_mday << endl;

   // Lasketaan päivämäärä 100 päivää eteenpäin
   now->tm_mday += 100;
   mktime(now);

   // Tulostetaan päivämäärä
   cout << "100 päivän päästä on " << (now->tm_year + 1900) << '-' 
        << (now->tm_mon + 1) << '-'
        << now->tm_mday << endl;

   return 0;
}
```

### Sample Output:

```
Tänään on 2021-1-1
100 päivän päästä on 2021-4-11
```

## Syvä sukellus

Kuten koodiesimerkissä näkyy, voidaan päivämääriä muokata käyttämällä C++:n aikafunktioita, kuten `time()` ja `localtime()`. Näiden avulla saadaan päivämäärä muutettua tietojen jaksoittain, esimerkiksi vuodesta, kuukaudesta ja päivästä. Lisäksi funktiota `mktime()` käytetään päivämäärän laskemiseen eteen- tai taaksepäin halutun ajanjakson verran. On tärkeää huomata, että tällainen päivämäärien laskeminen ei ole täysin tarkka välttämättä esimerkiksi huomioimatta karkausvuotta tai kesäaikaa.

## Katso myös

- [C++ Reference - Date and Time functions](https://www.cplusplus.com/reference/ctime/) 
- [C++ Aika ja päivämäärä](https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm)
- [Päivämäärän laskeminen C++](https://www.techiedelight.com/calculate-date-tomorrow-yesterday-cpp/)