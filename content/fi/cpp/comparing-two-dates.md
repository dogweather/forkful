---
title:                "C++: Kahden päivämäärän vertailu"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi vertailla kahta päivämäärää?

Vertailu kahden päivämäärän välillä voi olla tärkeää, kun haluat tarkistaa, onko tietty päivämäärä ennen vai jälkeen toisen. Se voi olla hyödyllistä esimerkiksi projektien aikataulutuksessa tai tietyn tapahtuman päivämäärän tarkistamisessa.

## Miten vertailla kahta päivämäärää?

Vertaamme kahta päivämäärää käyttämällä C++:n aikojen ja päivämäärien kirjastoa. Seuraavassa esimerkissä tarkastelemme, onko annettu päivämäärä ennen vai jälkeen toista päivämäärää.

```C++
#include <iostream>
#include <ctime>
 
using namespace std;
 
int main() {
   // Luodaan kaksi aikarakennetta
   struct tm date1 = {0, 0, 0, 1, 3, 120}; // 1.3.2020
   struct tm date2 = {0, 0, 0, 10, 6, 120}; // 10.6.2020
 
   // Muunnetaan aikarakenteet time_t -objekteiksi
   time_t time1 = mktime(&date1);
   time_t time2 = mktime(&date2);
 
   // Vertaillaan aikakoordinaatteja
   if (time1 < time2)
      cout << "Päivämäärä 1 on ennen päivämäärää 2.";
   else if (time1 > time2)
      cout << "Päivämäärä 1 on jälkeen päivämäärää 2.";
   else
      cout << "Päivämäärät ovat samat.";
 
   return 0;
}
```

Tulostus:
```
Päivämäärä 1 on ennen päivämäärää 2.
```

## Syvempi sukellus

Päivämäärä- ja aikafunktiot C++:ssa ovat tärkeitä työkaluja, kun haluat hallita ja vertailla päivämääriä ohjelmoinnissa. `mktime()`-funktio muuntaa aikarakenteen `struct tm` time_t -objektiksi, jota voidaan sitten verrata muihin aikaobjekteihin. On myös mahdollista käyttää muita aikafunktioita, kuten `difftime()`, joka laskee ajaneroa kahden ajan välillä.

## Katso myös

- [cppreference - Aikafunktiot](https://en.cppreference.com/w/cpp/chrono)
- [cplusplus - Aikafunktioilla työskentely](https://www.cplusplus.com/reference/ctime/)