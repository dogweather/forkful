---
title:    "C: Kirjoittaminen standardi virheelle"
keywords: ["C"]
---

{{< edit_this_page >}}

## Miksi?

Kun ohjelmoidaan C:llä, käytämme usein tavanomaisia tulostusfunktioita, kuten printf (), tulostamaan haluamamme tiedot konsolille. Mutta joskus meidän on otettava käyttöön jotain erilaista - kuten kirjoittaminen standardivirheeseen. Tässä blogipostissa kerron sinulle, miksi ja miten kirjoittaa standardivirheeseen.

## Miten?

Kirjoittaminen standardivirheeseen tapahtuu helposti käyttämällä fprintf () -funktiota ja osoittamalla sille parametriksi standardivirheen FILE-muuttuja stderr. Tässä on yksinkertainen esimerkki:

```C
#include <stdio.h>

int main() {
  fprintf(stderr, "Tämä on esimerkki tekstistä standardivirheeseen!");
  return 0;
}
```

## Syventyvä sukellus

Kirjoittaminen standardivirheeseen antaa meille mahdollisuuden ohittaa odottamattomia virheitä ja virheilmoituksia ohjelmamme suorituksen aikana. Voimme myös käyttää sitä palauttamaan tietoa käyttäjälle tai tallentamaan ohjelman suorituksen kannalta tärkeitä tietoja. On kuitenkin tärkeää käyttää standardivirhettä harkitusti ja tarvittaessa ohjelman suorituksen nopeuttamiseksi.

## Katso myös

- [Virheenkäsittelyn perusteet C:llä](https://www.tutorialspoint.com/cprogramming/c_error_handling.htm)
- [Standardivirheen selitys Stack Overflow'ssa](https://stackoverflow.com/questions/26028944/explanation-of-stderr-and-stdout-in-c)