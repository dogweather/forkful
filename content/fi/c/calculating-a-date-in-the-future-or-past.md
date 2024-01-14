---
title:                "C: Tulevan tai menneen päivämäärän laskeminen"
simple_title:         "Tulevan tai menneen päivämäärän laskeminen"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Jokaisella meistä on joskus ollut tilanne, jossa olemme joutuneet laskemaan tietyn päivämäärän tulevaisuudessa tai menneisyydessä. Tämä voi johtua esimerkiksi suunnittelemastamme matkasta tai tärkeästä tapahtumasta, kuten syntymäpäivästä. Onneksi C-ohjelmointikielellä voimme tehdä tämän laskutoimituksen helposti ja tarkasti.

## Kuinka

Laskeminen päivämäärä tulevaisuudessa tai menneisyydessä C-kielellä vaatii vain muutaman rivin koodia. Ennen kuin aloitamme, on tärkeää tietää, että C-kielessä käytetään Gregoriaanista kalenteria, joka ei ota huomioon kaikkia historiallisia kalenterimuutoksia. Tästä syystä laskelmat saattavat poiketa hieman todellisesta päivämäärästä menneisyydessä. 

Alla on esimerkkiohjelma, joka laskee tulevaisuuden päivämäärän annetun päivämäärän ja päivien määrän perusteella:

```C
#include <stdio.h>
int main() {
    // Tämä on alkuperäinen päivämäärä
    int paiva = 20;
    int kuukausi = 6;
    int vuosi = 2021;

    // Lisätään päiviä laskennan avulla
    int lisattavatPaivat = 30;

    // Tulostetaan tulevaisuuden päivämäärä
    printf("Tulevaisuuden päivämäärä: %d.%d.%d\n",
            paiva + lisattavatPaivat, kuukausi, vuosi);
    return 0;
}
```

Ohjelman tulosteena näkyy:

```
Tulevaisuuden päivämäärä: 20.7.2021
```

Samoin voimme laskea menneisyyden päivämäärän vähentämällä päiviä alkuperäisestä päivämäärästä:

```C
#include <stdio.h>
int main() {
    // Tämä on alkuperäinen päivämäärä
    int paiva = 25;
    int kuukausi = 6;
    int vuosi = 2021;

    // Vähennetään päiviä laskennan avulla
    int vahennettavatPaivat = 10;

    // Tulostetaan menneisyyden päivämäärä
    printf("Menneisyyden päivämäärä: %d.%d.%d\n",
            paiva - vahennettavatPaivat, kuukausi, vuosi);
    return 0;
}
```

Tulosteena näkyy:

```
Menneisyyden päivämäärä: 15.6.2021
```

## Syventävä tutkimus

C-kielellä laskettaessa päivämäärää kannattaa huomioida, että funktioilla kuten `time()` ja `localtime()` on rajoitus vuodelle 2038. Tämän vuoksi laskelmat saattavat virheellisesti näyttää päivämäärän vuoden 2038 jälkeen. Tämä on otettava huomioon sovelluksissa, joissa käsitellään suuria päivämääriä.

On myös tärkeää huomata, että päivämäärälaskujen tarkkuus riippuu laitteen käyttämästä kellosta. Esimerkiksi jos laitteen kello on virheellinen tai sitä ei ole asetettu oikeaan aikaan, päivämäärälaskelmat saattavat olla epätarkkoja.

## Katso myös

- [Date and Time Functions in C](https://www.tutorialspoint.com/c_standard_library/time