---
title:                "C: Tarkista löytyykö kansio"
simple_title:         "Tarkista löytyykö kansio"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

Monissa C-ohjelmoinnin projekteissa on tarvetta tarkistaa, onko tietokoneen tiedostojärjestelmässä olemassa tiettyä hakemistoa. Tämä voi johtua esimerkiksi tarpeesta luoda uusia tiedostoja tai varmistaa, että tietyt tiedostot ovat olemassa ennen niiden käyttöä.

## Näin teet sen

Tiedostojärjestelmän hakemistojen tarkistaminen voidaan toteuttaa helposti C-kielellä käyttämällä `opendir()` ja `closedir()` -funktioita. Ensimmäinen funktio avaa halutun hakemiston ja palauttaa siihen liittyvän osoittimen, kun taas jälkimmäinen sulkee hakemiston ja vapauttaa sen käytöstä.

```C
#include <stdio.h>
#include <dirent.h>

int main() {
  // Avataan haluttu hakemisto
  DIR *dir = opendir("/polku/hakemistoon/");

  // Tarkistetaan, onko hakemisto olemassa
  if (dir) {
    // Hakemisto löytyi, tulostetaan viesti
    printf("Hakemisto on olemassa!\n");
    // Suljetaan hakemisto
    closedir(dir);
  }
  else {
    // Hakemisto ei löytynyt, tulostetaan virheviesti
    printf("Hakemistoa ei löytynyt!\n");
  }

  return 0;
}
```

Esimerkin tulostus:

```
Hakemisto on olemassa!
```

## Syvemmälle aiheeseen

Tiedostojärjestelmän hakemistojen tarkistaminen on tärkeä ja hyödyllinen osa C-ohjelmointia. On kuitenkin tärkeää huomioida, että `opendir()` ja `closedir()` -funktiot toimivat vain Unix/Linux-järjestelmissä, kun taas esimerkiksi Windowsissa käytetään `FindFirstFile()` ja `FindClose()` -funktioita vastaaviin tarkoituksiin.

Lisäksi on hyvä huomioida, että `opendir()` voi palauttaa osoittimen myös epäonnistumisen sattuessa, esimerkiksi jos hakemistoon ei ole käyttöoikeuksia. Tästä syystä on tärkeää tarkistaa myös mahdollinen virheilmoitus.

## Katso myös

- [opendir() -dokumentaatio](https://pubs.opengroup.org/onlinepubs/009696699/functions/opendir.html)
- [closedir() -dokumentaatio](https://pubs.opengroup.org/onlinepubs/009696699/functions/closedir.html)
- [How to check if a directory exists in C](https://www.geeksforgeeks.org/how-to-check-if-a-directory-or-a-file-exists-in-system) (englanniksi)