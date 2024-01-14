---
title:                "C: Päivämäärän muuntaminen merkkijonoksi"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmointiprojekteissa on tarve muuttaa päivämäärä merkkijonoksi. Tähän voi olla monia syitä, kuten päiväyksen näyttäminen käyttäjälle tai datan tallentaminen tietokantaan. Jotkut kielet tarjoavat sisäänrakennettuja toimintoja päivämäärän muuttamiseen merkkijonoksi, mutta C-kielessä tätä ei ole.

## Miten

Päivämäärän muuttaminen merkkijonoksi C-kielessä vaatii hieman enemmän työtä kuin muissa kielissä. Ensimmäinen askel on luoda päivämäärää edustava rakenne, esimerkiksi käyttäen `struct tm` -rakennetta. Sitten voimme käyttää `strftime()` -funktiota muuttamaan tämän rakenneeseen tallennetun päivämäärän merkkijonoksi halutussa muodossa.

```
#include <stdio.h>
#include <time.h>

int main() {
    // Luodaan päivämäärää edustava rakenne
    struct tm date = { .tm_year = 2021, 
                        .tm_mon  = 7,
                        .tm_mday = 30 };

    // Muutetaan päivämäärä merkkijonoksi
    char dateString[11]; // tarpeeksi pitkä pvm-merkkijonolle
    strftime(dateString, 11, "%d.%m.%Y", &date);

    // Tulostetaan merkkijono
    printf("Päivämäärä muodossa DD.MM.YYYY: %s\n", dateString);

    return 0;
}
```

Tämä koodi tuottaa seuraavan tulosteen:

```
Päivämäärä muodossa DD.MM.YYYY: 30.07.2021
```

On tärkeää huomata, että `strftime()` hyväksyy useita erilaisia muotoilumerkkejä eri päivämäärän osien muotoiluun. Esimerkiksi `%d` tarkoittaa päivää, `%m` kuukautta ja `%Y` vuotta. Voit löytää lisää erilaisia muotoilumerkkejä [täältä](https://www.cplusplus.com/reference/ctime/strftime/).

## Syvällinen sukellus

Päivämäärän muuttamisen merkkijonoksi takana on oikeastaan kaksi tärkeää käsitettä: päivämäärän esittäminen ja merkkijonojen käsittely.

Päivämäärän esittäminen on tärkeä asia, sillä meidän täytyy varmistaa, että päivämäärä näytetään halutussa muodossa. Tämä tarkoittaa esimerkiksi oikean päivämääräjärjestyksen ja erottimien käyttöä. Merkkijonojen käsittely puolestaan tarkoittaa, että meidän täytyy varata tarpeeksi tilaa merkkijonolle ja tarkistaa, ettei se ylitä maksimipituutta.

On myös tärkeää huomata, että C-kielessä olevat päivämäärämuodot ovat hieman erilaisia kuin esimerkiksi JavaScriptissä tai Pythonissa. Tämä tarkoittaa, että jos siirrät koodia toisesta kielestä, päivämäärän muotoilu voi vaatia hieman muuntamista.

## Katso myös

- [strftime() -funktio C-kielen dokumentaatiossa](https://www.cplusplus.com/reference/ctime/strftime/)
- [Päivämäärän esittäminen ja merkkijonojen käsittely C-kielessä](https://www.geeksforgeeks.org/converting-strings-numbers-structures-c/)
- [Esimerkkejä päivämäärän