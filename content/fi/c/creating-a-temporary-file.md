---
title:    "C: Väliaikaisen tiedoston luominen"
keywords: ["C"]
---

{{< edit_this_page >}}

## Miksi luoda tilapäistiedosto?

Tilapäistiedostoja käytetään usein ohjelmoinnissa tietojen tallentamiseen lyhytaikaisesti. Tämä voi olla hyödyllistä esimerkiksi, kun halutaan pitää kirjaa käytössä olevista tiedostoista, muokata tietoa ennen sen tallentamista lopulliseen tiedostoon tai tallentaa väliaikaisesti tietoa, jota tarvitaan vain tietyn osan ohjelmasta aikana.

## Näin luot tilapäistiedoston C-ohjelmassa

```
#include <stdio.h>
#include <stdlib.h>

int main() {

    // Luodaan tiedostonimi muuttuja, jossa on uniikki nimi
    char* tiedostonimi = tmpnam(NULL);

    // Avataan tiedosto
    FILE* tiedosto = fopen(tiedostonimi, "w");

    // Tarkistetaan, onko tiedosto avattu onnistuneesti
    if (tiedosto == NULL) {
        printf("Virhe: tiedoston avaaminen epäonnistui\n");
    } else {

        // Kirjoitetaan tietoa tiedostoon
        fprintf(tiedosto, "Tämä on tilapäistiedosto\n");

        // Suljetaan tiedosto
        fclose(tiedosto);
    }

    // Poistetaan tilapäistiedosto
    remove(tiedostonimi);

    return 0;
}
```
Tässä esimerkissä näet, miten voit luoda uniikin tiedostonimen `tmpnam()` -funktiolla ja avata tiedoston `fopen()` -funktiolla. Voit sitten käyttää tavallisia tiedoston käsittelyfunktioita tiedon tallentamiseen ja käsittelemiseen. Lopuksi voit poistaa tiedoston `remove()` -funktiolla.

## Syvenny tarkemmin tilapäistiedostojen luomiseen

Tilapäistiedostojen luominen on tärkeä osa C-ohjelmointia, mutta se voi myös aiheuttaa haasteita. Tässä muutamia vinkkejä, joilla voit välttää yleisimmät ongelmat:

- Käytä `tmpnam()` -funktiota uniikin tiedostonimen luomiseen sen sijaan, että luot itse tiedostonimen.
- Varmista, että poistat tilapäistiedoston, kun se ei enää ole tarpeen. Näin voit välttää levytilan turhaa käyttöä.
- Jos mahdollista, käytä `mkstemp()` -funktiota tilapäistiedoston luomiseen sen sijaan, että luotat `tmpnam()` tai `tmpfile()` -funktioihin. `mkstemp()` tarjoaa enemmän hallintaa tiedostonimen ja tiedostotilan suhteen.

## Katso myös

- [tmpnam](https://www.cplusplus.com/reference/cstdio/tmpnam/)
- [fopen](https://www.cplusplus.com/reference/cstdio/fopen/)
- [remove](https://www.cplusplus.com/reference/cstdio/remove/)