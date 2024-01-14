---
title:                "C++: Nykyisen päivämäärän saaminen"
simple_title:         "Nykyisen päivämäärän saaminen"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

Monesti ohjelmointiprojekteissa tarvitaan nykyisen päivämäärän käyttöä. Tämä on erityisen hyödyllistä ajan merkitsemiseen esimerkiksi lokiin tai tiedostonumerointiin.

## How To

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main(){
    // Haetaan nykyinen aika
    std::chrono::system_clock::time_point now = std::chrono::system_clock::now();
    
    // Muunnetaan aika tavalliselle muodolle
    std::time_t current_time = std::chrono::system_clock::to_time_t(now);
    
    // Tulostetaan nykyinen päivämäärä
    std::cout << "Nykyinen päivämäärä: " << std::ctime(&current_time) << std::endl;
    
    return 0;
}
```
Tulostus:
```
Nykyinen päivämäärä: Tue Jun 29 14:34:46 2021
```

## Deep Dive

Nykyisen päivämäärän saaminen vaatii aikaleimojen käsittelyä. C++ tarjoaa tähän apuna `chrono` kirjaston, jolla voidaan käsitellä aikoja tarkasti ja tehokkaasti. `std::chrono::system_clock` luokan avulla voidaan hakea nykyinen aika, jonka jälkeen se muunnetaan helposti halutulle muodolle.

## Katso myös
- [cppreference.com - std::chrono](https://en.cppreference.com/w/cpp/chrono)
- [cplusplus.com - Working with time](https://www.cplusplus.com/reference/ctime/)
- [cplusplus.com - std::chrono::system_clock](https://www.cplusplus.com/reference/chrono/system_clock/)