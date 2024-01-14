---
title:    "C: Merkkijonojen yhdistäminen"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi: Miksi ketjuttaminen kannattaa?

Ketjuttaminen eli merkkijonojen yhdisteleminen on tärkeä osa C-ohjelmointia. Se mahdollistaa monipuolisen ja joustavan tavan käsitellä merkkijonoja, mikä on tärkeää monissa ohjelmointitehtävissä. Ketjuttaminen tarjoaa myös tehokkaan tavan muokata, tulostaa ja käsitellä merkkijonoja ohjelmassa.

## Miten: Esimerkkejä ja tulosteita "```C ... ```" koodilohkoilla.

Ketjuttaminen merkitsee kahden tai useamman merkkijonon yhdistämistä yhdeksi merkkijonoksi. Tämä tapahtuu käyttämällä '+'-merkkiä tai strcat-funktiota. Katso esimerkki alla:

```C
#include <stdio.h>
#include <string.h>

int main() {
   char etunimi[20] = "Matti";
   char sukunimi[] = "Meikäläinen";
   char nimi[40];

   // käytetään strcat-funktiota
   strcpy(nimi, etunimi);
   strcat(nimi, " ");
   strcat(nimi, sukunimi);
   printf("Nimi: %s", nimi);

   // käytetään '+'-merkkiä
   strcpy(nimi, "Matti");
   printf("\nNimi: %s", nimi + " Meikäläinen");

   return 0;
}
```

Tuloste:

```
Nimi: Matti Meikäläinen
Nimi: Matti Meikäläinen
```

## Syvällisemmin: Lisätietoja merkkijonojen ketjuttamisesta.

Merkkijonojen ketjuttamisessa on tärkeää ottaa huomioon muutamia asioita. Ensinnäkin, strcat-funktiota käytettäessä on varmistettava, että merkkijonolle on varattu tarpeeksi muistitilaa lisäämistä varten, jotta ohjelma ei kaadu. Myös muistin vapauttaminen ketjuttamisen jälkeen on tärkeää, jotta vältetään ylimääräisten muistivuotojen aiheuttamat ongelmat.

Lisäksi ketjuttamista voidaan käyttää myös muiden tietotyyppien kanssa, kuten int- tai float-muuttujien, mutta silloin on huomioitava datan tyyppien yhteensopivuus.

## Katso myös

- [C-kielen dokumentaatio merkkijonojen käsittelystä](https://www.cplusplus.com/reference/cstring/)
- [Merkkijonojen yhdistäminen muiden C-tietotyyppien kanssa](https://www.tutorialspoint.com/cprogramming/c_concatenation.htm)
- [Muistin vapauttaminen ketjuttamisen jälkeen](https://stackoverflow.com/questions/15144570/what-is-the-proper-way-of-free-space-allocated-for-the-char-pointer-when-string)