---
title:                "Tilapäistiedoston luominen"
html_title:           "C: Tilapäistiedoston luominen"
simple_title:         "Tilapäistiedoston luominen"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Mikä & miksi?
Temporaalitiedostojen luominen on yksi yleisimpiä ohjelmointitekniikoita, jota käytetään tallentamaan tietoja väliaikaisesti ohjelman suorituksen aikana. Tämä auttaa saavuttamaan paremman suorituskyvyn ja toiminnallisuuden, ja siksi se on suosittu valinta monien ohjelmoijien keskuudessa.

## Kuinka:
```
#include <stdio.h>
#include <stdlib.h>

int main(){
  FILE *temp_file; //luodaan tiedoston osoitin
  char temp_file_name[20]; // luodaan merkkijono nimelle
  temp_file = tmpfile(); // luodaan väliaikainen tiedosto
  if(temp_file == NULL) { // tarkistetaan onko tiedoston luominen onnistunut
    printf("Tiedoston luominen epäonnistui!");
    exit(1);
  }
  printf("Väliaikaisen tiedoston nimi on: %s", temp_file_name);
  fclose(temp_file); // suljetaan tiedosto
  return 0;
}
```
```
Väliaikaisen tiedoston nimi on: /tmp/tmpyoeHSM

```

## Syväsukellus:
Temporaaliset tiedostot kehitettiin alun perin käyttöjärjestelmien, kuten Unixin, yhteydessä. Ne ovat edelleen tärkeä osa modernia ohjelmointia, ja niitä käytetään usein muun muassa tietokantojen väliaikaisena tallennustilana. Vaihtoehtoisia tapoja luoda temporaalisia tiedostoja ovat esimerkiksi ```tempnam``` ja ```mktemp``` funktiot, mutta ```tmpfile``` on yleisesti pidetty ohjelmointitekniikka.

## Katso myös:
[Unix-tiedostotyypit ja niiden käyttöönotto C:ssä](https://www.tutorialspoint.com/unix/unix-file-types.htm)
[C-kielet ja tiedostonhallinta](https://www.programiz.com/c-programming/c-file-input-output)
[Kuinka luodaan väliaikaisia tiedostoja C:ssä](https://www.geeksforgeeks.org/temporary-files-c-programming/)