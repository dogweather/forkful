---
title:                "C: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Miksi Vertailla Kahden Päivämäärän Välillä?

Vertailemalla kahta päivämäärää voidaan selvittää eri päivämäärien välisiä suhteita ja mahdollisesti tehdä päätelmiä esimerkiksi ajankäytöstä tai ajanhallinnasta. Tämä voi olla hyödyllistä esimerkiksi projektinhallinnassa tai aikataulutuksessa.

## Miten Vertailla Kahden Päivämäärän Välillä?

Vertaamalla kahta päivämäärää voidaan käyttää C-ohjelmointikielen sisäänrakennettuja funktioita, kuten `difftime()`, joka laskee kahden annetun päivämäärän välisen ajan eron sekunneissa. Seuraavassa esimerkissä käytämme `difftime()`-funktiota laskemaan kuinka monta päivää on kulunut uudenvuodenpäivästä tänään.

```C
#include <stdio.h>
#include <time.h>

int main() {
    // Alustetaan uudenvuodenpäivän aika rakenne
    struct tm uusi_vuosi = {0};
    uusi_vuosi.tm_year = 121; // Vuonna 2021
    uusi_vuosi.tm_mon = 0; // Tammikuussa
    uusi_vuosi.tm_mday = 1; // Ensimmäinen päivä

    // Selvitetään tämän päivän aika
    time_t t = time(NULL);
    struct tm* aika = localtime(&t);

    // Lasketaan päivien ero käyttäen difftime() -funktiota
    double erotus = difftime(mktime(aika), mktime(&uusi_vuosi)) / (60 * 60 * 24);

    printf("Kuinka monta päivää on kulunut uudenvuodenpäivästä tähän päivään: %d päivää.\n", (int) erotus);

    return 0;
}
```

**Tulostus:**
```
Kuinka monta päivää on kulunut uudenvuodenpäivästä tähän päivään: 215 päivää.
```

## Syvällisemmin Kahden Päivämäärän Vertailusta

C-ohjelmointikielen sisäänrakennettujen funktioiden lisäksi voidaan myös käyttää omia funktioita päivämäärien vertailuun. Esimerkiksi seuraavassa funktiossa tarkastellaan kahta annettua päivämäärää ja vertaillaan niiden vuosia, kuukausia ja päiviä.

```C
int vertaa_pvm(struct tm pvm1, struct tm pvm2) {

    // Vertaa vuosia
    if(pvm1.tm_year > pvm2.tm_year) {
        return 1;
    } else if(pvm1.tm_year < pvm2.tm_year) {
        return -1;
    }

    // Vertaa kuukausia
    if(pvm1.tm_mon > pvm2.tm_mon) {
        return 1;
    } else if(pvm1.tm_mon < pvm2.tm_mon) {
        return -1;
    }

    // Vertaa päiviä
    if(pvm1.tm_mday > pvm2.tm_mday) {
        return 1;
    } else if(pvm1.tm_mday < pvm2.tm_mday) {
        return -1;
    }

    // Päivämäärät ovat samoja
    return 0;
}
```

Tämä funktio palauttaa arvon `1`, mikäli ensimmäinen annettu päivämäärä (`pvm1`) on myöhäisempi kuin toinen annettu päivämäärä (`pvm2`). Arvo `-1` palautetaan, jos `pvm1` on aikaisempi kuin `pvm2`. Jos päivämäärät ovat samoja, funktio palauttaa `0`.

## Katso Myös

- [C -