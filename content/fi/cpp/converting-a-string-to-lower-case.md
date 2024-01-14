---
title:                "C++: Merkkijonon muuntaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

On monia tilanteita, joissa haluat muuttaa merkkijonon pieniksi kirjaimiksi ohjelmasi suorituskyvyn tai käytön helpottamiseksi. Tämä voi sisältää esimerkiksi vertailuja tai tiettyjen merkkijonojen etsimistä isommasta merkkijonosta. Tässä blogikirjoituksessa opimme, kuinka tehdä tämä C++:ssa muutamalla yksinkertaisella askeleella.

## Kuinka tehdä

Merkkijonon muuttaminen pieniksi kirjaimiksi C++:ssa on helppoa käyttäen standardikirjaston toimintoa `transform`. Ensimmäiseksi meidän täytyy sisällyttää `algorithm`-kirjasto, jotta voimme käyttää tätä toimintoa. Sitten voimme käyttää `transform`-funktiota ja antaa sille parametreiksi merkkijonon ja funktion, joka muuttaa merkkijonon jokaisen kirjaimen pieneksi.

```C++
#include <algorithm>
#include <iostream>

int main() {
    std::string s = "TÄMÄ ON MERKKIJONO";
    std::transform(s.begin(), s.end(), s.begin(), tolower);
    std::cout << s << std::endl;
    return 0;
}

// tulostaa "tämä on merkkijono"
```

## Syvällinen selitys

`transform`-funktio käy läpi annetun merkkijonon ja kutsuu annettua funktiota jokaiselle merkkijonon kirjaimelle. Funktion tulisi palauttaa muutettu kirjain, joka tallennetaan takaisin merkkijonoon. Käytämme tässä `tolower`-funktiota, joka muuttaa annetun kirjaimen pieneksi.

On tärkeää huomata, että `transform`-funktio muuttaa annetun merkkijonon suoraan eikä luo uutta merkkijonoa. Tämä tarkoittaa, että voimme tallentaa muutetun merkkijonon takaisin alkuperäiseen muuttujaan.

## Katso myös

- [`transform`-funktion dokumentaatio (cplusplus.com)](https://www.cplusplus.com/reference/algorithm/transform/)
- [Enemmän esimerkkejä merkkijonojen käsittelystä C++:ssa (programiz.com)](https://www.programiz.com/cpp-programming/string)