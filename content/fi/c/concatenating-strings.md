---
title:                "C: Töiden yhdistäminen"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/concatenating-strings.md"
---

{{< edit_this_page >}}

# Miksi

Stringien liittäminen tai konkatenointi on tärkeä osa C-ohjelmointia, sillä se mahdollistaa useampien merkkijonojen yhdistämisen yhdeksi kokonaisuudeksi. Tämä kätevä toiminto helpottaa merkkijonojen käsittelyä ja mahdollistaa monimutkaistenkin tehtävien suorittamisen.

## Miten

Stringien liittäminen C-kielellä onnistuu helposti käyttämällä sisäänrakennettua funktiota `strcat()`. Tämä funktio toimii yksinkertaisesti lisäämällä toisen merkkijonon ensimmäisen loppuun ja palauttamalla yhdistetyn merkkijonon. Alla on esimerkki koodista ja sen tulosteesta:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str1[50] = "Tämä on ";
    char str2[] = "merkkijonojen";
    printf("%s%s", str1, strcat(str1, str2));
    return 0;
}
```
*Tuloste: Tämä on merkkijonojen*

Huomaa, että `strcat()` ei luo uutta merkkijonoa, vaan lisää merkkijonon `str1` loppuun. Jos haluat luoda uuden merkkijonon, joka sisältää kaksi yhdistettyä merkkijonoa, voit käyttää `sprintf()`-funktiota, kuten alla olevassa esimerkissä:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str1[50] = "Tämä on ";
    char str2[] = "merkkijonojen";
    char str3[100];
    sprintf(str3, "%s%s", str1, str2);
    printf("%s", str3);
    return 0;
}
```
*Tuloste: Tämä on merkkijonojen*

On myös tärkeää huomata, että merkkijonot tulee määrittää tarpeeksi suuriksi, jotta ne mahtuvat yhdistettyyn merkkijonoon. Muuten voi tapahtua tietojen ylikirjoitusta ja ohjelma saattaa kaatua.

## Syvempi sukellus

C-kielessä merkkijonot ovat taulukoita, joiden viimeinen merkki on aina `\0`. Tämä nollamerkki toimii merkkijonon loppumerkkinä ja mahdollistaa merkkijonon käsittelyn C:ssä. Kun käytät `strcat()`-funktiota, se etsii merkkijonon viimeistä `\0`-merkkiä ja lisää toisen merkkijonon sen jälkeen.

Vaikka konkatenointi onkin kätevä toiminto, se on myös aikaa vievä ja tehoton tapa käsitellä merkkijonoja. Mikäli tarvitset suurta määrää merkkijonojen yhdistämistä, voi olla parempi käyttää dynaamisia merkkijonoja, joiden käsittely on paljon nopeampaa.

# Katso myös

- [strcat() opetusohjelma](https://www.geeksforgeeks.org/strcat-function-in-c/)
- [sprintf() opetusohjelma](https://www.geeksforgeeks.org/sprintf-in-c/)
- [C-merkkijonot opetusohjelma](https://www.tutorialspoint.com/cprogramming/c_strings.htm)