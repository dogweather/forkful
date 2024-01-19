---
title:                "Päivämäärän muuttaminen merkkijonoksi"
html_title:           "Go: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Muunnetaan päivämäärä merkkijonoksi C-ohjelmoinnissa

## Mikä & Miksi?
Päivämäärän muuntaminen merkkijonoksi tarkoittaa päivämääräarvon esittämistä ihmisen luettavassa tekstimuodossa. Ohjelmoijat tekevät tämän yleensä päivämäärien mukavampaa esittämistä, tallennusta tai vertailua varten.

## Kuinka:
Tässä on koodiesimerkki, jolla voi muuntaa päivämäärän merkkijonoksi C -kielellä.

```C
#include <time.h>
#include <stdio.h>
#include <string.h>

int main() {
    char muotoiltu_paivamaara[100];
    time_t aika;
    struct tm *AikaInfo;

    // Haetaan nykyhetken aika
    time(&aika);
    AikaInfo = localtime(&aika);
 
    // Muotoillaan aika merkkijonoksi
    strftime(muotoiltu_paivamaara, 100, "%d.%m.%Y", AikaInfo);

    printf("Päivämäärä merkkijonona: %s\n", muotoiltu_paivamaara);

    return 0;
}
```
Esimerkin ajon tulostus saattaa näyttää seuraavalta:

```
Päivämäärä merkkijonona: 12.04.2022
```

## Syvempi sukellus
Vaikka päivämäärän muuntaminen merkkijonoksi C-kielessä on suoraviivaista `strftime()` funktion avulla, on hyvä huomata, että eri standardit saattavat vaikuttaa tulosten formaattiin. Esimerkiksi ISO 8601 määrittelee päivämäärän representoinnin "vvvv-kk-pp".

Pidemmän historian C-ohjelmointi on toteuttanut tämän ominaisuuden POSIX-standardin mukaisesti. POSIX määrittelee `strftime()`-funktion, jota yleisesti käytetään C-kielessä.

On mainittava, että `strftime()` ei ole ainoa tapa tehdä tämä muunnos. `sprintf()` funktiota voidaan myös käyttää muotoiluun, mutta se ei tue automaattista kuukauden, päivän ja vuoden muotoilua, kuten `strftime()`.

## Katso myös
C-kielen päivämäärän ja ajan hallintaan liittyviä linkkejä:
1. GNU:n ohje `strftime`:lle: https://www.gnu.org/software/libc/manual/html_node/Formatting-Calendar-Time.html#Formatting-Calendar-Time
2. C `time.h` kirjasto: https://en.cppreference.com/w/c/chrono
3. ISO 8601 standardi: https://en.wikipedia.org/wiki/ISO_8601