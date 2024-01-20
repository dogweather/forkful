---
title:                "Tilapäisen tiedoston luominen"
html_title:           "Arduino: Tilapäisen tiedoston luominen"
simple_title:         "Tilapäisen tiedoston luominen"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Väliaikaistiedostojen luominen on prosessi, jossa ohjelmisto tallentaa tiedot väliaikaisesti tiedostoon, jonka se myöhemmin poistaa. Tämä on hyödyllistä, kun käsitellään suuria tietoja, joiden tallennusta pysyvästi ei tarvita.

## Miten Tehdä:

Tässä on yksinkertainen esimerkkikoodi väliaikaisen tiedoston luomiseksi C-ohjelmoinnissa.

```C
#include <stdio.h>

int main() {
    FILE * tmpf = tmpfile();
    fprintf(tmpf, "Hei, Suomi!\n");
    rewind(tmpf);
    char buf[20];
    fgets(buf, sizeof(buf), tmpf);
    printf("%s", buf); // Tulostaa: Hei, Suomi!
    return 0;
}
```
Kun tämän ohjelman suoritus päättyy, se myös automaattisesti poistaa luomansa väliaikaisen tiedoston.

## Syvä Sukellus:

Väliaikaisten tiedostojen luomiskäytäntö on peräisin ajalta, jolloin tietokoneen muisti oli rajallinen. Ohjelmat käyttävät sitä yhä tänään, erityisesti suurien tiedostojen käsittelyssä tai tiedostojen luonnissa, joita ei tarvitse säilyttää pitkään.

Myös muita vaihtoehtoja on olemassa. In-memory -tiedostojärjestelmät, kuten `/dev/shm` Linuxissa, ovat yksi esimerkki.

Suurten tiedostojen käsittelyssä väliaikaistiedostoa tulisi käyttää joko luomalla ainutlaatuisia tiedostonimiä `mkstemp` -funktiolla tai käyttämällä `tmpfile` -funktiota, joka luo anonyymin väliaikaistiedoston, kuten ylläolevassa esimerkissämme.

## Katso Myös:

1. [ISO C -standardin dokumentaatio tmpfile](https://www.cplusplus.com/reference/cstdio/tmpfile/)
2. [Man-sivut: tmpfile (3) - Linux-manuaalinen sivu](https://man7.org/linux/man-pages/man3/tmpfile.3.html)
3. [C Standard Library](https://en.wikipedia.org/wiki/C_standard_library)