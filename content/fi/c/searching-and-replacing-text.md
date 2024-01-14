---
title:    "C: Tekstin etsiminen ja korvaaminen"
keywords: ["C"]
---

{{< edit_this_page >}}

## Miksi

Tekstin etsiminen ja korvaaminen ovat välttämättömiä toimintoja monissa ohjelmointiprojekteissa. Ne mahdollistavat tietyn tekstin tai merkkijonon nopean ja helpon muuttamisen toiseksi. Tämä on erityisen hyödyllistä, kun käsitellään suuria määriä tekstitiedostoja, joissa samanlaista tekstiä esiintyy useita kertoja.

## Miten tehdä

C-ohjelmoinnissa tekstin etsiminen ja korvaaminen tapahtuvat yleensä käyttämällä merkkijonojen tai merkkien käsittelyyn tarkoitettuja funktioita. Yksi tällainen funktio on `strstr()`, joka etsii tietyn merkkijonon esiintymän toisesta merkkijonosta. Alla on esimerkki:

```C
#include <stdio.h>
#include <string.h>

int main()
{
  char teksti[] = "Tämä on esimerkkiteksti.";
  char etsittava[] = "esimerkki";

  char *osoitin = strstr(teksti, etsittava);

  if (osoitin != NULL) {
    printf("Ensimmäinen esiintymä löytyi kohdasta %d. \n", osoitin - teksti + 1);
  } else {
    printf("Ei löytynyt esiintymää. \n");
  }

  return 0;
}
```

Tulostus olisi seuraava:

```
Ensimmäinen esiintymä löytyi kohdasta 11.
```

Muita hyödyllisiä funktioita ovat esimerkiksi `strchr()`, joka etsii ensimmäisen tietyn merkin esiintymän merkkijonosta, ja `strtok()`, joka jakaa merkkijonon osiin tietyn erotinmerkin avulla. Nämä ja muut vastaavat funktiot löytyvät `string.h`-kirjastosta.

## Syvällisemmin

Tekstin etsiminen ja korvaaminen voivat olla hyödyllisiä myös, kun käsitellään käyttäjän syöttämiä merkkijonoja. Esimerkiksi ohjelman tulee usein varmistaa, että käyttäjän syöttämä merkkijono ei sisällä tiettyjä kiellettyjä merkkejä tai sanoja. Tällöin voidaan käyttää `strcmp()` tai `strtol()` -funktioita, jotka vertailevat merkkijonoja ja mahdollistavat esimerkiksi kiellettyjen merkkien poistamisen.

Sekä tekstin etsiminen että korvaaminen ovat myös tärkeitä osia tiedostonkäsittelyä. Kun halutaan muuttaa tietyn tiedoston sisältöä, voidaan käydä läpi tiedosto ja korvata halutut merkkijonot toisilla. Tämä on erityisen kätevää, jos tietystä tiedostosta halutaan poistaa tiettyjä sanoja tai korvata niitä uusilla.

## Katso myös

- [String Function Examples in C](https://www.programiz.com/c-programming/c-strings-functions)
- [C String Functions Reference](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [Learn C - Strings](https://www.learn-c.org/en/Strings)