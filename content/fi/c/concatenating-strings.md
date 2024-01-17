---
title:                "Merkkijonojen yhdistäminen"
html_title:           "C: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/concatenating-strings.md"
---

{{< edit_this_page >}}

Mitä & Miksi?
Stringien yhdistäminen on prosessi, jossa useita merkkijonoja liitetään yhdeksi merkkijonoksi. Tätä tehdään yleensä, kun halutaan luoda monimutkaisempia tekstejä, kuten lauseita tai tiedostonimiä. Ohjelmoijat käyttävät tätä toimintoa tehostamaan koodinsa toimintaa.

Miten:
Alla on esimerkkejä siitä, miten stringien liittämistä voidaan käyttää C-kielellä. Huomaa, että käytämme standardia ```printf()``` -funktiota tulostamaan liitettyjä merkkijonoja terminaaliin.

```C
#include <stdio.h>
#include <string.h>

int main() {
  char string1[] = "Hello";
  char string2[] = "World";
  char result[20];

  // Yksinkertainen tapa:
  strcat(result, string1);
  strcat(result, string2);
  
  printf("Tulostus 1: %s\n", result);
  
  // Tehokkaampi tapa, jossa ei käytetä alustavallokaatiota:
  sprintf(result, "%s%s", string1, string2);
  
  printf("Tulostus 2: %s\n", result);
  return 0;
}
```

Tulostus 1: HelloWorld

Tulostus 2: HelloWorld

Syväluotaus:
Stringien liittäminen on ollut yleinen ohjelmointikäytäntö jo pitkään. Ennen C-kieltä on käytetty esimerkiksi PERL-ohjelmointikielessä, joka on kehitetty 1980-luvun lopulla. C-kielessä yksittäisten merkkien lisäksi voidaan myös liittää kokonaisia merkkijonoja, mikä tekee siitä tehokkaan vaihtoehdon muihin koodikieliin verrattuna.

On myös olemassa muita vaihtoehtoja stringien liittämiseen, kuten käyttämällä ```strncat()```-funktiota, jonka avulla voidaan rajoittaa liitettävien merkkien määrää. Tämä voi olla hyödyllistä, jos halutaan estää merkkijonojen ylittämät pituusrajoitukset.

Stringien liittämistä voidaan myös toteuttaa manuaalisesti käyttämällä ```for``` -silmukkaa ja kopiointifunktiota, kuten ```strcpy()```. Tämä voi kuitenkin olla hankalampaa ja aikaa vievämpää kuin C:n valmiiden funktioiden käyttö.

Katso myös:
- [Official C documentation](https://devdocs.io/c/)
- [Learn C in Y Minutes](https://learnxinyminutes.com/docs/c/)
- [String Concatenation in C by GeeksforGeeks](https://www.geeksforgeeks.org/string-concatenation-in-c/)