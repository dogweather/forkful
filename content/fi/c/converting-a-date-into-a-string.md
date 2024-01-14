---
title:    "C: Päivämäärän muuttaminen merkkijonoksi"
keywords: ["C"]
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit muuttaa päivämäärän merkkijonoksi? Nimensä mukaisesti *muuntaa* päivämäärän merkkijonoksi, jotta voit näyttää sen käyttäjälle tai tallentaa sen tiedostoon. Päivämäärän muotoilu on tärkeä osa monien ohjelmien toimintaa, joten on tärkeää ymmärtää, kuinka se tehdään.

## Kuinka

```C
#include <stdio.h>
#include <time.h>

int main() {
    // Luo aikarakente
    time_t t = time(NULL);
    
    // Muuntaa ajan rakenteesta merkkijonoksi
    char date[64];
    strftime(date, sizeof(date), "%d/%m/%Y", localtime(&t));
    
    printf("Tänään on: %s\n", date);
    
    return 0;
}

```

Tässä esimerkissä käytämme `time` ja `strftime` funktioita luomaan päivämäärästä merkkijonon. Voit muuttaa päivämäärän muotoa muokkaamalla `strftime` funktion parametreja.

Osoite muodostaa-merkkijono funktion dokumentaatiota saadaksesi lisätietoja sen toiminnasta ja käytöstä.

## Syvemmälle

Päivämäärän muuntaminen merkkijonoksi voi olla monimutkainen prosessi, varsinkin jos haluat tukea useita eri lokalisointeja ja aikavyöhykkeitä. On tärkeää ymmärtää, miten koodi käsittelee aikaa ja miten muutokset voivat vaikuttaa lopputulokseen. Suosittelemme lukemaan C-kielellä kirjoitettuja päivämäärän muuntamisen dokumentaatioita ja tutkimaan ohjelmointikielen ominaisuuksia, jotta voit syventää ymmärrystäsi tästä aiheesta.

## Katso myös

- [C strftime Dokumentaatio](https://en.cppreference.com/w/c/chrono/strftime)
- [Opas C-kieleen](https://fi.wikipedia.org/wiki/C_(ohjelmointikieli))