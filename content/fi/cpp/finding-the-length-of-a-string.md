---
title:                "Merkkijonon pituuden selvittäminen"
html_title:           "C++: Merkkijonon pituuden selvittäminen"
simple_title:         "Merkkijonon pituuden selvittäminen"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

String-muuttujan pituuden selvittäminen on tärkeää, koska se auttaa ohjelmoijaa käsittelyyn käytettävän muistin hallinnassa. Lisäksi se mahdollistaa tietojen tarkastamisen ja manipuloinnin tarkasti ja nopeasti.

## Miten

```C++
#include <iostream>
#include <string>

using namespace std;

int main(){

  // Määritellään string-muuttuja
  string s = "Hei maailma!";

  // Selvitetään merkkijonon pituus ja tulostetaan se
  int length = s.length();
  cout << "Merkkijonon pituus on: " << length << endl;

  return 0;
}
```

**Tuloste:**
```
Merkkijonon pituus on: 12
```

## Syvempi sukellus

String-muuttujien pituuden selvittäminen tapahtuu käyttämällä "length" -funktiota C++:ssa. Tämä funktio palauttaa merkkijonon pituuden kokonaislukuna. **Huomaa**, että tämä funktio ei laske mukaan merkkijonon loppuun lisättyä jäsentä.

Lisäksi on olemassa muita vaihtoehtoisia tapoja saada selville string-muuttujan pituus, kuten "size" -funktio tai "for" -silmukan käyttäminen. On hyvä tutustua eri vaihtoehtoihin ja valita paras tapa jokaiseen tilanteeseen.

## Katso myös

- [C++ stringien käyttö](https://www.ohjelmointiputka.net/c-plus-plus/c-plus-plus-stringit.php)
- [C++ string-muuttujien määrittely](https://www.ohjelmointiputka.net/c-plus-plus/c-plus-plus-muuttujat.php#string)
- [C++ "length" -funktio](https://www.cplusplus.com/reference/string/string/length/)