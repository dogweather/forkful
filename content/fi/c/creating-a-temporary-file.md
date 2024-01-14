---
title:                "C: Väliaikaisen tiedoston luominen"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi luoda väliaikaistiedostoja?

Väliaikaisten tiedostojen luominen on yleinen tekniikka, jota käytetään ohjelmoinnissa tiedon tallentamiseen hetkellisesti. Näitä tiedostoja voidaan käyttää esimerkiksi silloin, kun halutaan tallentaa väliaikaisia laskentatuloksia tai kun ohjelma tarvitsee väliaikaisen tallennustilan.

## Näin luot väliaikaistiedoston C-kielellä

Väliaikaistiedostojen luominen C-kielellä on yksinkertaista käyttämällä standardikirjaston <stdio.h> ja <stdlib.h> funktioita. Alla on esimerkki, joka näyttää, miten luodaan väliaikainen tiedosto ja tallennetaan siihen merkkijono.

```C
#include <stdio.h>
#include <stdlib.h>

int main(void) {
  FILE *file;
  char string[50];

  // Luodaan väliaikainen tiedosto, joka avataan kirjoitustilassa
  file = tmpfile();

  // Tarkistetaan, onko tiedosto luotu onnistuneesti
  if (file == NULL) { 
    printf("Väliaikaistiedoston luominen epäonnistui");
    exit(1);
  }

  // Kysytään käyttäjältä merkkijono ja tallennetaan se väliaikaiseen tiedostoon
  printf("Syötä haluamasi merkkijono: ");
  scanf("%s", string);
  fprintf(file, "%s", string);

  // Suljetaan tiedosto ja tulostetaan sen sisältö
  fclose(file);
  printf("Väliaikaistiedoston sisältö: ");
  system("cat /tmp/<tiedoston_nimi>");

  return 0;
}
```
Kun koodi suoritetaan, käyttäjältä kysytään merkkijono, joka tallennetaan väliaikaiseen tiedostoon. Tiedoston sisältö tulostetaan lopuksi komentorivillä funktion "cat" avulla.

## Syvemmälle väliaikaistiedostojen maailmaan

Väliaikaistiedostot luodaan yleensä silloin, kun tiedon tallentaminen muistiin ei ole optimaalista tai mahdollista. Nämä tiedostot luodaan usein eri ohjelmien väliseen kommunikaatioon. Väliaikaisten tiedostojen luominen on myös yleinen tekniikka, jota käytetään esimerkiksi web-palvelimien välimuistien hallinnassa.

Väliaikaistiedostot voidaan myös luoda nimellä, mikäli halutaan tallentaa tietoa tiettyyn paikkaan tiedostojärjestelmässä. Näitä tiedostoja voidaan käsitellä kuten muitakin tiedostoja, ja ne tulee muistaa poistaa käytön jälkeen.

## Katso myös

- [The Basics of Creating Temporary Files in C](https://www.guru99.com/c-temporary-file.html)
- [Temporary file](https://en.wikipedia.org/wiki/Temporary_file)