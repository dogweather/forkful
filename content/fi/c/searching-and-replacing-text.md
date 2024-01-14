---
title:                "C: Hakeminen ja tekstin korvaaminen"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Tekstin etsintä ja korvaaminen on tärkeä osa C-ohjelmointia. Se mahdollistaa tekstissä olevien tiettyjen termien tai muotojen korvaamisen toisilla, mikä helpottaa koodin muokkaamista ja ylläpitämistä. Tämä blogikirjoitus auttaa sinua ymmärtämään, kuinka tämä prosessi toimii ja miten voit hyödyntää sitä omassa koodissasi.

## Miten tehdä

Tekstin etsintä ja korvaaminen C-kielellä onnistuu käyttämällä 'str' -perheen funktioita, kuten `strchr` ja `strstr`. Näiden funktioiden avulla voit etsiä haluamasi merkkijonon ja korvata sen toisella. Katso esimerkki alla olevasta koodiblokista:

```C
#include <stdio.h>
#include <string.h>

int main(void) {
    char teksti[] = "Hei maailma!";
    char *esiintyma;

    esiintyma = strchr(teksti, 'm');
    *esiintyma = 'k';

    printf("%s", teksti); // Tulostaa "Hei kaailka!"
    return 0;
}
```

Tässä esimerkissä käytämme `strchr` funktiota löytämään ensimmäisen esiintymän merkille 'm' ja korvaamme sen kirjaimella 'k'. Tämän jälkeen tulostamme uuden muokatun tekstin, "Hei kaailka!".

Voit myös käyttää `strstr` funktiota etsimään tiettyä merkkijonoa ja korvaamaan sen toisella merkkijonolla. Katso esimerkki alla:

```C
#include <stdio.h>
#include <string.h>

int main(void) {
    char teksti[] = "Tämä on esimerkkiteksti";
    char *esiintyma;

    esiintyma = strstr(teksti, "esimerkki");
    strcpy(esiintyma, "uusi");
    
    printf("%s", teksti); // Tulostaa "Tämä on uusi teksti"
    return 0;
}
```

Tässä käytämme `strstr` funktiota löytämään tekstin "esimerkki" ja korvaamme sen tekstillä "uusi". Tuloksena saamme "Tämä on uusi teksti".

## Syvempi sukellus

C-kielessä tekstien etsiminen ja korvaaminen tapahtuu merkkijonojen manipuloinnin avulla. Mutta on tärkeää huomata, että merkkijonot ovat loppujen lopuksi vain merkkitaulukoita, joissa jokainen merkki vastaa tiettyä numerokoodia. Tämän vuoksi on tärkeää olla tarkkaavainen, kun käsitellään merkkijonoja, jotta varmistetaan, että ohjelma toimii oikein.

Jos haluat oppia lisää merkkijonojen manipuloinnista C-kielessä, voit tutustua näihin resursseihin:

- [C String Handling](https://www.tutorialspoint.com/cprogramming/c_strings.htm)
- [Working with strings in C](https://www.geeksforgeeks.org/working-with-strings-in-c/)
- [Using string functions in C](https://www.cprogramming.com/tutorial/string.html)


## Katso myös

- [Merkkijonon etsiminen ja korvaaminen C++:ssa](https://www.tutorialcup.com/cplusplus/replace-string-c-programming.htm)
- [Regex-etsintä ja korvaaminen C:ssä](https://www.tutorialspoint.com/regex/regex_string_handling.htm)
- [C-kielen dokumentaatio merkkijonofunktioille](https://www.tutorialspoint.com/c_standard_library/c_function_strstr.htm)