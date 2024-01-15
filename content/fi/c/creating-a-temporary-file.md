---
title:                "Väliaikaisen tiedoston luominen"
html_title:           "C: Väliaikaisen tiedoston luominen"
simple_title:         "Väliaikaisen tiedoston luominen"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi luoda väliaikaisen tiedoston C-ohjelmoinnissa? Väliaikaiset tiedostot voivat olla hyödyllisiä esimerkiksi silloin, kun tarvitaan tallennustilaa väliaikaista tietoa varten tai kun halutaan varmistaa, että data ei jää pysyvästi käyttöjärjestelmän muistiin.

## Kuinka

Tässä osiossa esittelemme käytännön esimerkkejä siitä, kuinka voit luoda väliaikaisen tiedoston C-ohjelmoinnissa.

```C
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h> // tarvitaan mkstemp-toimintoa varten

int main() {
  // Luo väliaikainen tiedosto "tempfile"
  char *tempfile = mktemp("tempfile");

  // Avaa tiedosto kirjoitustilassa
  FILE *fp = fopen(tempfile, "w");

  // Tarkista onnistuiko tiedoston luominen
  if (fp == NULL) {
    printf("Väliaikaisen tiedoston luominen epäonnistui");
    exit(1);
  }

  // Kirjoita data tiedostoon
  fprintf(fp, "Tämä on väliaikainen tiedosto!");

  // Sulje tiedosto
  fclose(fp);

  // Tulosta tiedoston sisältö
  printf("Luotu tiedosto %s sisältää seuraavan tekstin:\n", tempfile);
  system("cat tempfile");

  return 0;
}
```

Tässä esimerkissä käytämme ```mktemp```-funktiota luodaksemme väliaikaisen tiedoston ja avataksesi sen kirjoitustilassa. Tämän jälkeen voit kirjoittaa tiedostoon haluamasi datan ja sulkea sen. Lopuksi käytämme ```cat```-komentoa tulostaaksemme tiedoston sisällön konsoliin.

```C
// Output:
// Luotu tiedosto tempfile sisältää seuraavan tekstin:
// Tämä on väliaikainen tiedosto!
```

Voit myös käyttää ```tmpnam```-funktiota luodaksesi väliaikaisen tiedoston, jos et halua itse antaa nimeä tiedostolle.

```C
#include <stdio.h>
#include <stdlib.h>

int main() {
  // Luo väliaikainen tiedostonimi
  char *tempfile = tmpnam(NULL);

  // Avaa tiedosto kirjoitustilassa
  FILE *fp = fopen(tempfile, "w");

  // Tarkista onnistuiko tiedoston luominen
  if (fp == NULL) {
    printf("Väliaikaisen tiedoston luominen epäonnistui");
    exit(1);
  }

  // Kirjoita data tiedostoon
  fprintf(fp, "Tämä on väliaikainen tiedosto!");

  // Sulje tiedosto
  fclose(fp);

  // Tulosta tiedoston sisältö
  printf("Luotu tiedosto %s sisältää seuraavan tekstin:\n", tempfile);
  system("cat tempfile");

  return 0;
}
```

Tässä esimerkissä käytämme ```tmpnam```-funktiota luodaksemme väliaikaisen tiedoston ja avataksesi sen kirjoitustilassa. Muilla parametreilla voit muokata luodun tiedoston nimeä haluamillasi tavoilla.

```C
// Output:
// Luotu tiedosto /tmp/filerl4g6 sisältää seuraavan tekstin:
// Tämä on väliaikainen tiedosto!
```

## Syvemmälle

Edellä esitellyt esimerkit ovat vain pintaraapaisu siitä, kuinka voit luoda väliaikaisia tiedostoja C-ohjelmoinnissa. Vaihtoehtoisia tapoja on useita ja ne tarjoavat erilaisia ominaisuuksia ja toiminnallisuuksia. Kannattaa tutustua myös ```mkstemp```, ```tmpfile``` ja ```tmpnam```-funktioihin saadaksesi lisät