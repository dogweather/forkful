---
title:    "C: Kirjoittaminen standardivirheelle"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi Kirjoittaa Virheet Standardilähtöön?

Kirjoittaminen standardilähtöön on tärkeä osa C-ohjelmointia, sillä se antaa ohjelmoijalle mahdollisuuden lähettää virheitä ja muita ilmoituksia suoraan terminaaliin. Tämä voi auttaa käyttäjiä selvittämään ohjelmassa mahdollisesti ilmenneitä ongelmia ja auttaa ohjelmoijaa löytämään ja korjaamaan virheitä.

## Miten Kirjoittaa Virheet Standardilähtöön?

Kirjoittaminen virheitä ja muita ilmoituksia standardilähtöön tapahtuu käyttämällä C-kieleen sisäänrakennettua "fprintf" -funktiota. Se ottaa parametreikseen tiedostonimen, johon viesti kirjoitetaan, ja itse viestin. Katso alla olevaa esimerkkiä:

```C
#include <stdio.h>

int main() {
    FILE *virhe_tiedosto = fopen("virhe.txt", "w"); // Avaamme tiedoston kirjoitustilassa
    if (virhe_tiedosto != NULL) {
        fprintf(virhe_tiedosto, "Virhe: Tiedostoa ei löydy!\n"); // Kirjoitamme viestin tiedostoon
    }
    fclose(virhe_tiedosto); // Suljemme tiedoston
    return 0;
}
```

Ajaessa tämän koodin saat luotua tiedoston nimeltä "virhe.txt", johon on kirjoitettu "Virhe: Tiedostoa ei löydy!". Tämä esimerkki on vain yksinkertainen tapa käyttää "fprintf" -funktiota, ja mahdollisuuksia on paljon enemmän. Voit esimerkiksi lähettää viestejä standardilähtöön käyttämällä "stderr" -muuttujaa. Katso alla oleva esimerkki:

```C
#include <stdio.h>

int main() {
    fprintf(stderr, "Virhe: Tiedostoa ei löydy!\n"); // Lähetämme viestin standardilähtöön
    return 0;
}
```

Tämä vaihtoehto on hyödyllinen silloin, kun haluat lähettää viestin suoraan terminaaliin, esimerkiksi kun ohjelma kohtaa virheen.

## Syvällisempi Tarkastelu Kirjoittamisesta Standardilähtöön

Kirjoittaminen standardilähtöön on tärkeä osa C-ohjelmointia, sillä se mahdollistaa virheiden ja muiden ilmoitusten lähettämisen terminaaliin. Tämä voi auttaa käyttäjiä selvittämään ohjelmassa mahdollisesti ilmenneitä ongelmia ja auttaa ohjelmoijaa löytämään ja korjaamaan virheitä. Lisäksi kirjoittaminen standardilähtöön antaa ohjelmoijalle mahdollisuuden luoda interaktiivisia ohjelmia, jotka kommunikoivat käyttäjän kanssa suoraan terminaalin kautta. Esimerkiksi "scanf" -funktio lukee käyttäjän antamia syötteitä standardilähtöstä.

## Katso Myös

- [C-kirjasto: stdio.h](https://www.cs.cf.ac.uk/Dave/C/node9.html)
- [Ymmärtäminen standardi-IO](http://www.csl.mtu.edu/cs4411.ck/www/NOTES/non-local-jump/node6.html)
- [Käyttäjän interaktio terminaalin kautta C:llä](http://www.prismnet.com/~eggert/index.c.html#tools)