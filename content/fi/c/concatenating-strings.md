---
title:                "Lisämerkkien yhdistäminen"
html_title:           "C: Lisämerkkien yhdistäminen"
simple_title:         "Lisämerkkien yhdistäminen"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

On monia tilanteita, joissa haluat yhdistää useita merkkijonoja yhdeksi. Tämä voi olla tarpeen esimerkiksi tulostettaessa tietoja käyttäjälle tai luotaessa dynaamisia SQL-kyselyitä tietokantaan. 

## Kuinka tehdä se

Voit yhdistää merkkijonoja käyttämällä standardikirjaston `strcat()` -funktiota, joka vaatii kaksi merkkijonoa parametreina. Se liittää toisen merkkijonon ensimmäiseen ja palauttaa lopputuloksen. Toinen vaihtoehto on käyttää `sprintf()` -funktiota, joka luo uuden merkkijonon yhdistämällä halutut merkkijonot ja muut objektit samalla kertaa.

```C
char s1[20] = "Tämä ";
char s2[] = "on lause.";
strcat(s1, s2); // s1 = "Tämä on lause."

char s3[50];
char nimi[] = "Jaska";
int ika = 30;
sprintf(s3, "Hei, olen %s ja olen %d vuotta vanha.", nimi, ika); // s3 = "Hei, olen Jaska ja olen 30 vuotta vanha."
```

## Syvällisempää tietoa

Merkkijonojen yhdistäminen vaatii tiettyä varovaisuutta, sillä liian pitkät merkkijonot voivat aiheuttaa muistin ylikirjoituksen, mikä voi johtaa odottamattomiin tuloksiin. Siksi on tärkeää varmistaa, että tulosmerkkijonolle on varattu riittävästi tilaa ja käytetään tarvittaessa turvallisia kirjastofunktioita, kuten `strncat()` ja `snprintf()`.

Merkkijonon yhdistäminen voi myös hidastaa ohjelman suoritusta, jos sitä tehdään suurille datamäärille. Tällöin voi olla hyödyllistä hajottaa yhdistäminen useisiin osiin ja käyttää esimerkiksi `sprintf()`-funktiota vain lopullisen merkkijonon luomiseen. Lisäksi kannattaa huolehtia merkkijonojen oikeasta formaatoinnista, jotta lopputulos olisi odotetunlainen.

## Katso myös

- [strcat() dokumentaatio (englanniksi)](https://www.cplusplus.com/reference/cstring/strcat/)
- [sprintf() dokumentaatio (englanniksi)](https://www.cplusplus.com/reference/cstdio/sprintf/)
- [strncat() dokumentaatio (englanniksi)](https://www.cplusplus.com/reference/cstring/strncat/)
- [snprintf() dokumentaatio (englanniksi)](https://www.cplusplus.com/reference/cstdio/snprintf/)