---
title:    "C: Merkkijonon pituuden löytäminen"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

On useita syitä, miksi lasketaan tekstin pituus C-ohjelmoinnissa. Yksi yleisimmistä käyttötarkoituksista on tarkistaa, onko teksti allekirjoitetulle muuttujalle annetun enimmäispituuden rajoissa. Tämä on erityisen tärkeää, kun käsitellään käyttäjältä saatuja syötteitä, jotta varmistetaan ohjelman suoritus ilman virheitä.

## Miten tehdä

Tekstin pituuden laskeminen C-ohjelmoinnissa on melko suoraviivaista. Käytämme `strlen()` -funktiota, joka on osa `string.h` -kirjastoa. Alla on yksinkertainen koodiesimerkki:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char teksti[] = "Tämä on esimerkkiteksti";
    int pituus = strlen(teksti);

    printf("Tekstin pituus on: %d merkkiä", pituus);
    return 0;
}
```

Koodin suoritus antaa seuraavan tulosteen:

`Tekstin pituus on: 23 merkkiä`

## Syvempää tietoa

C-ohjelmointikielen `strlen()` -funktio toimii siten, että se laskee vastaantulevan merkin indeksin alusta loppuun asti. Otetaan esimerkiksi edellinen koodiesimerkki:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char teksti[] = "Tämä on esimerkkiteksti";
    int pituus = strlen(teksti);

    printf("Tekstin pituus on: %d merkkiä", pituus);

    printf("\nErikoismerkkejä: ");
    for(int i=0; i<pituus; i++){
        printf(" %c(%d) ", teksti[i], teksti[i]);
    }

    return 0;
}
```

Tässä esimerkissä olemme lisänneet koodiin toisen `printf` -rivin, joka tulostaa käytetyn tekstin erikoismerkit ja niiden vastaavat numerokoodit. Tuloste näyttää tältä:

```
Tekstin pituus on: 23 merkkiä
Erikoismerkkejä: T(84) ä(228) m(109) ä(228)  (32) o(111) n(110)  (32) e(101) s(115) i(105) m(109) e(101) r(114) k(107) k(107) i(105) t(116) e(101) k(107) s(115) t(116) i(105)
```

Kuten näemme, `strlen()` -funktio laskee myös välilyönnin ja erikoismerkit osaksi tekstin pituutta.

## Katso myös

- [stdlsenl():n dokumentaatio C-kielessä](https://www.tutorialspoint.com/c_standard_library/c_function_strlen.htm)
- [C-koodiesimerkkejä - Tekstin kääntäminen ja laskeminen](https://www.programiz.com/c-programming/examples/string-length)
- [C-kieliohjelmointi - Tekstin käsittely](https://www.tutorialspoint.com/cprogramming/c_strings.htm)

Kiitos lukemisesta! Toivottavasti tämä blogikirjoitus on auttanut sinua ymmärtämään tekstin pituuden laskemisen tärkeyttä C-ohjelmoinnissa. Muista käyttää `strlen()` -funktiota kaikissa tarvittavissa tilanteissa, jotta voit varmistaa tekstin oikean käsittelyn. Onnea ohjelmoinnissa!