---
title:    "C: Merkkijonojen yhdistäminen"
keywords: ["C"]
---

{{< edit_this_page >}}

## Miksi

Miksi perehtyä merkkijonojen yhdistämiseen ohjelmoinnissa? Merkkijonojen yhdistäminen on tärkeä osa ohjelmoinnin maailmaa, ja se mahdollistaa monien tehtävien suorittamisen, kuten tekstin muokkaamisen ja tulostuksen.

## Miten

Merkkijonojen yhdistäminen onnistuu helposti käyttäen C-ohjelmoinnissa "strcat" -funktiota, joka yhdistää kaksi merkkijonoa toisiinsa. Tämän avulla voit luoda uuden merkkijonon yhdistämällä useita pienempiä merkkijonoja.

```C
#include <stdio.h>
#include <string.h>

int main()
{
    char str1[50] = "Tervetuloa!";
    char str2[20] = "Opi ";
    char str3[20] = "C-ohjelmointia";

    strcat(str2, str3);
    strcat(str1, str2);

    printf("%s", str1);
    return 0;
}
```

Tämän koodin tuloste on: "Tervetuloa! Opi C-ohjelmointia".

## Syvempi sukellus

Merkkijonojen yhdistäminen ei vain yksinkertaisesti liitä merkkijonoja yhteen, vaan se käyttää taustalla monimutkaisempia algoritmeja löytääkseen sopivan tilan uudelle merkkijonolle. Tästä syystä on tärkeää muistaa varata tarpeeksi tilaa uudelle merkkijonolle ennen yhdistämisen suorittamista.

Lisäksi tarvittaessa voit käyttää muita merkkijonojen yhdistämiseen tarkoitettuja funktioita, kuten "strncpy" tai "strncat", jotka mahdollistavat tietyn määrän merkkien kopiointia tai yhdistämistä.

## Katso myös

- [C-ohjelmoinnin perusteet](https://fi.wikipedia.org/wiki/C_(ohjelmointikieli))
- [C-kielen merkkijonojen manipulointi](https://www.tutorialspoint.com/cprogramming/c_strings.htm)
- [Strcat ja muut merkkijonojen yhdistämiseen tarkoitetut funktiot](https://www.programiz.com/c-programming/library-function/string.h/strcat)