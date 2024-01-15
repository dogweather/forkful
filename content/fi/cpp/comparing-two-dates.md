---
title:                "Kahden päivämäärän vertailu"
html_title:           "C++: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi

Olet ehkä joskus tarvinnut tarkistaa, ovatko kaksi päivämäärää sama, ovatko ne samassa järjestyksessä vai kuinka kaukana ne ovat toisistaan. Tässä artikkelissa näytämme, miten voit vertailla kahta päivämäärää C++:ssa.

## Miten

Vertailemalla kahta päivämäärää voimme käyttää C++:n valmiita funktioita, jotka on tarkoitettu päivämäärien käsittelyyn.

```C++
#include <iostream>
#include <ctime>

int main() {
  // Luo kaksi tm-tyyppistä rakennetta
  tm date1 = {0, 0, 0, 1, 0, 2020 - 1900};
  tm date2 = {0, 0, 0, 1, 0, 2021 - 1900};
  
  // Muunna pysyvät struct rakenteeksi time_t
  time_t time1 = mktime(&date1);
  time_t time2 = mktime(&date2);
  
  // Vertaa aikaleimoja ja tulosta halutut tiedot
  if (time1 == time2) {
    std::cout << "Päivämäärät ovat samat" << std::endl;
  }
  else if (time1 < time2) {
    std::cout << "Ensimmäinen päivämäärä on aikaisempi" << std::endl;
  }
  else if (time1 > time2) {
    std::cout << "Toinen päivämäärä on aikaisempi" << std::endl;
  }
  
  // Laske päivien erotus
  double difference = difftime(time2, time1) / (60 * 60 * 24);
  std::cout << "Päivien erotus: " << difference << std::endl;
  
  return 0;
}
```

**Tuloste:** *Toinen päivämäärä on aikaisempi* ja *Päivien erotus: 366*

## Syventävä katsaus

C++:ssa päivämäärät ovat tavallisesti kokonaisluvuiksi muunnettuja aikaleimoja, jotka esittävät sekunteja vuoden alusta. Tämän avulla voimme helposti verrata kahta päivämäärää muuntamalla ne aikaleimoiksi ja käyttää sitten vertailuoperaattoreita tai erityisiä difftime-funktiota saadaksemme halutun tiedon.

## Katso myös

- [C++ Date and Time handling](https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm)
- [mktime(CppReference)](https://en.cppreference.com/w/cpp/chrono/c/mktime)
- [difftime(CppReference)](https://en.cppreference.com/w/cpp/chrono/c/difftime)