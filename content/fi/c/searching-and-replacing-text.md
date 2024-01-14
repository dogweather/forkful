---
title:    "C: Tekstin etsiminen ja korvaaminen"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi joku saattaisi haluta etsiä ja korvata tekstiä ohjelmointityössään. Ehkä tiettyä tekstipätkää käytetään usein ja halutaan helpottaa sen muuttamista, tai ehkä tietyt muuttujat pitää päivittää jatkuvasti eri projekteissa. Riippumatta syystä, C-ohjelmointikielessä on olemassa hyvät työkalut tekstikorvausten tekemiseen.

## Miten tehdä

Etsimisen ja korvaamisen tekeminen C-ohjelmointikielessä on hyvin yksinkertaista, ja useimmat ohjelmoijat ovat sen jo oppineet perusohjelmoinnin kurssilla. Perusmalli on seuraava:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char text[] = "Tervetuloa, maailma!";
    char search[] = "Tervetuloa";
    char replace[] = "Hei";

    // Etsitään tekstistä etsittävä teksti
    char *result = strstr(text, search);

    // Korvataan teksti käyttämällä strcpy-funktiota
    strcpy(result, replace);

    printf("%s", text);
    return 0;
}
```

Tässä esimerkissä haetaan "Tervetuloa" tekstistä ja korvataan se "Hei"-tekstillä. Lopputuloksena tulostuu "Hei, maailma!". 

## Syvällinen tarkastelu

On tärkeää huomata, että strstr-funktio etsii vain tekstin ensimmäisen esiintymän. Jos tekstissä on useita samanlaisia kohtia, ne eivät korvata. Tämä voidaan kiertää käyttämällä esimerkiksi while-silmukkaa ja käsittelemällä tekstiä yksi merkki kerrallaan. Lisäksi C-kielen stringien manipulointiin on muitakin tehokkaita funktioita, kuten strtok ja strncmp.

On myös tärkeää käsitellä korvattavan tekstin pituus oikein, jotta sitä ei korvata liikaa tai liian vähän. Esimerkiksi strncpy-funktio kopioidaan vain määritellyn määrän merkkejä, joten se on hyödyllinen jos korvattava teksti on pidempi kuin uuden tekstin. Jos taas halutaan vain lisätä tekstiä olemassa olevan tekstin jatkoksi, käytetään strcat-funktiota.

## Katso myös

- [C-kielen merkkijonojen käsittely](https://www.tutorialspoint.com/cprogramming/c_strings.htm)
- [Stringin käsittely C:ssä](https://www.geeksforgeeks.org/string-handling-strings-c-2/)
- [Strstr man-sivu](https://linux.die.net/man/3/strstr)