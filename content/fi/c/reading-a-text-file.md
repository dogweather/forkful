---
title:                "Tiedoston lukeminen"
html_title:           "C: Tiedoston lukeminen"
simple_title:         "Tiedoston lukeminen"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Tekstitiedoston lukeminen on prosessi, jossa ohjelma lukee tekstitiedostosta sisältöä ja tallentaa sen muistiin käsiteltäväksi. Ohjelmoijat käyttävät tätä toimintoa usein saadakseen tietoa tekstitiedostoista, kuten käyttäjän syötteistä tai tallennetuista tietokannoista.

## Näin teet:
```C
#include <stdio.h>

int main(){
  FILE *tiedosto;
  char merkki;
  tiedosto = fopen("tekstitiedosto.txt", "r");
  if(tiedosto == NULL){
      perror("Virhe tiedoston lukemisessa");
      return -1;
  }
  while((merkki = fgetc(tiedosto)) != EOF)
     printf("%c", merkki);
  fclose(tiedosto);
  return 0;
}
```
**Lähtö:**
```
Tämä on tekstitiedosto.
```

## Syvemmälle:
Tekstitiedoston lukeminen on ollut osa C-ohjelmointikieltä alusta asti ja se on yksi tärkeimmistä tavoista käsitellä tekstitiedostoja. Tiedostoa voidaan lukea myös lineaarisesti käyttäen esimerkiksi `fgets()`-funktiota. Lisäksi on olemassa lukuisia kirjastoja ja ohjelmia, jotka tarjoavat erilaisia tapoja lukea tekstitiedostoja.

## Katso myös:
- [fopen() - C:n virallinen dokumentaatio](https://www.cplusplus.com/reference/cstdio/fopen/)
- [Tekstitiedostojen käsittely C-kielellä - Artikkeli Suomi.dev-sivustolla](https://suomi.dev/tekstitiedostojen-kasittely-c-kielella/)