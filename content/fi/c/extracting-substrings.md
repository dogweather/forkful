---
title:    "C: Alimerkkijonojen erottelu"
keywords: ["C"]
---

{{< edit_this_page >}}

## Miksi

Substringien, eli osamerkkijonojen, erottaminen on hyödyllinen ohjelmointitekniikka, jota käytetään usein datan käsittelyssä ja muokkaamisessa. Tämä toiminto mahdollistaa tietyn osan merkkijonosta erottamisen, mikä voi nopeuttaa ja helpottaa koodin kirjoittamista.

## Kuinka

Substringien erottaminen C-kielellä on helppoa ja suoraviivaista. Koodausesimerkit ja niiden tulosteet on esitetty alla olevassa koodilohkossa.

```C
#include <stdio.h>
#include <string.h>

int main() {

  // Alustetaan merkkijono ja sen pituus
  char s[] = "Tervetuloa";
  int pituus = strlen(s);

  // Luodaan uusi merkkijono, johon substring tallennetaan
  char alaosa[pituus];

  // Käytetään strncpy-funktiota erottamaan osa merkkijonosta
  strncpy(alaosa, s+5, 4); // Aloittaa 6. merkistä ja erottaa 4 seuraavaa merkkiä
  alaosa[4] = '\0'; // Lisätään loppuun merkki, joka kertoo merkkijonon loppumisen

  // Tulostetaan lopputulos
  printf("Substring: %s", alaosa);
  return 0;
}
```

Tuloste:
```
Substring: luoa
```

## Syvälle sukellus

Substringien erottaminen toimii käyttämällä C:n sisäänrakennettuja merkkijonojen käsittelyfunktioita, kuten `strlen` ja `strncpy`. Näiden funktioiden avulla voimme määrittää merkkijonon pituuden ja kopioida osia merkkijonosta uuteen merkkijonoon. On tärkeää huolehtia, että uuteen merkkijonoon varataan tarpeeksi muistia, jotta siinä on tilaa kopioidulle osalle. Myös terminointimerkki `\0` on lisättävä loppuun, jotta merkkijono pysyy oikein muotoiltuna.

## Katso myös

- [C-kielen merkkijonot](https://www.cprogramming.com/tutorial/c/lesson9.html)
- [Substringi-funktion käyttö C:ssä](https://www.programiz.com/c-programming/library-function/string.h/strncpy)