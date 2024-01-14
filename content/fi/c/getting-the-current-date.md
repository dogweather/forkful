---
title:    "C: Nykyisen päivämäärän saaminen"
keywords: ["C"]
---

{{< edit_this_page >}}

## Miksi

Useat ohjelmat ja sovellukset käyttävät nykyään päivämäärää ja aikaa tietojen tallentamiseen ja näyttämiseen. Päivämäärän saaminen ohjelmassa on tärkeää, jotta voimme tietää, milloin jotain tapahtui tai milloin jotain on tallennettu. Se on myös hyödyllistä esimerkiksi kalenteri-sovelluksissa tai muistutusten asettamisessa tiettyyn päivämäärään. Siksi on tärkeää tietää, miten ohjelmassa saadaan nykyinen päivämäärä ja aika.

## Kuinka

Useimmissa ohjelmointikielissä on sisäänrakennettu funktio nykyisen päivämäärän ja ajan saamiseen. C-kielen tapauksessa käytämme `time.h`-kirjastoa ja sen sisältämiä funktioita. 

Ensimmäinen askel on sisällyttää `time.h`-kirjasto C-koodiimme. Tämän jälkeen voimme käyttää `time()`-funktiota saadaksemme nykyisen ajan ja tallentaa sen muuttujaan.

```C
#include <stdio.h>
#include <time.h>

int main() {
    // Muuttuja nykyisen ajan tallentamista varten
    time_t nykyinen_aika;

    // Käytetään time() saamaan nykyinen aika ja tallentamaan se muuttujaan
    time(&nykyinen_aika);

    // Käytetään ctime() muotoilemaan aika luettavaan muotoon
    printf("Nykyinen aika ja päivämäärä: %s", ctime(&nykyinen_aika));

    return 0;
}
```

Tulostus:

```
Nykyinen aika ja päivämäärä: Mon Apr 5 10:52:18 2021
```

## Syvempi sukellus

`time()`-funktio palauttaa ajan kuluneina sekunteina vuoden 1970 tammikuun 1. päivästä alkaen. Tämä aika tallennetaan `time_t`-muuttujaan. `ctime()`-funktio puolestaan muuntaa tämän ajan luettavaan muotoon, kuten esimerkissä.

On myös muita funktioita, kuten `localtime()`, joilla voimme muokata ja näyttää aikaa eri tavalla. Esimerkiksi `localtime()` palauttaa `struct tm`-tietorakenteen, joka sisältää tiedot tunnetusta ajasta. Tämän avulla voimme näyttää ajan esimerkiksi pelkästään tunnit ja minuutit tai päivämäärä ilman aikaa.

## Katso myös

- [C Language - Date and Time](https://www.tutorialspoint.com/c_standard_library/c_function_time.htm)
- [How to get current date and time in C?](https://www.geeksforgeeks.org/how-to-get-current-date-and-time-in-c/)