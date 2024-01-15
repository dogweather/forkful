---
title:                "Päivämäärän muuttaminen merkkijonoksi"
html_title:           "C++: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Yksi yleinen tarve C++ -ohjelmoijalle on muuttaa päivämäärä merkkijonoksi, esimerkiksi lähettääkseen sen eteenpäin tiedostojen tallentamiseen tai näyttääkseen käyttäjälle. Seuraavassa käsittelemme, kuinka tämän tehtävän voi suorittaa tehokkaasti ja helposti C++:lla.

## Kuinka tehdä

Date-luokka tarjoaa pääsyn useisiin jäsenmetodeihin, jotka helpottavat päivämäärän muuttamista merkkijonoksi.

```C++
#include <iostream>
#include <string>
#include <ctime>

// Luodaan uusi Date -instanssi ja asetetaan sille haluttu päivämäärä.
std::tm date = { 0, 0, 0, 20, 6, 2021 - 1900 };

// Muutetaan päivämäärä merkkijonoksi käyttäen asetettua mallia.
std::string str_date = std::put_time(&date, "%d.%m.%Y");

// Tulostetaan merkkijonoksi muutettu päivämäärä.
std::cout << str_date << std::endl;

// Output: 20.06.2021
```

Tässä esimerkissä käytetään `put_time` -funktiota, joka muuntaa päivämäärän haluttuun muotoon. Voit myös käyttää muita `put_time`-parametreja, kuten `%c` tulostamaan päivämäärän lyhyemmässä muodossa.

## Syvällisempi sukellus

Date-luokka tarjoaa myös muita hyödyllisiä jäsenmetodeja, kuten `day()`, `month()` ja `year()`, jotka palauttavat päivän, kuukauden ja vuoden arvot numeroina. Näitä arvoja voidaan käyttää muodostamaan päivämäärä haluttuun merkkijonoon sen sijaan, että käytettäisiin `put_time` -funktiota.

Voit myös muuttaa päivämäärän muotoa C++:lla käyttäen `strftime` -funktiota, joka toimii samalla tavalla kuin `put_time`.

## Katso myös

- [C++:n virallinen dokumentaatio | Date-luokka](https://en.cppreference.com/w/cpp/chrono/c/date)
- [C++:n Standardin kirjasto | päivämäärän muunnostapoja](https://en.cppreference.com/w/cpp/locale/time_put/put)