---
title:                "C: Muutetaan merkkijono pieniksi kirjaimiksi"
simple_title:         "Muutetaan merkkijono pieniksi kirjaimiksi"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisimme muuttaa merkkijonon pieniksi kirjaimiksi (string to lower case)? Yksi syy voi olla, että haluamme vertailla kahta merkkijonoa keskenään, ja isojen ja pienten kirjainten ero voi aiheuttaa ongelmia vertailussa. Tämän takia meidän täytyy muuntaa molemmat merkkijonot samassa muodossa, jotta vertailu olisi luotettavaa.

## Kuinka

```C
#include <stdio.h>
#include <ctype.h>

int main()
{
    // Luodaan merkkijono ja tallennetaan siihen arvo
    char string[] = "TÄMÄ ON MERKKIJONO";
    
    // "for" silmukassa käytetään "tolower" funktiota jokaiselle merkkijonon kirjaimelle
    for(int i = 0; string[i] != '\0'; i++)
    {
        string[i] = tolower(string[i]);
    }
    
    // Tulostetaan muokattu merkkijono
    printf("%s", string);
    
    return 0;
}
```
**Tulostus:**

tämä on merkkijono

## Syväsukellus

Kun haluamme muuttaa merkkijonon pieniksi kirjaimiksi, meidän täytyy käyttää "tolower" funktiota, joka tulee ctype.h kirjastosta. Tämä funktio muuttaa annetun merkin pieneksi kirjaimeksi, jos se on iso kirjain. Muulloin funktio palauttaa saman merkin. Tämän takia me suoritamme funktiota jokaiselle merkkijonon merkille "for" silmukassa, kunnes saavutamme merkkijonon lopun (joka merkitään '\0'). Näin saamme muutettua kaikki kirjaimet pieniksi kirjaimiksi.

## Katso myös

- [C-kielen "tolower" dokumentaatio](https://www.tutorialspoint.com/c_standard_library/c_function_tolower.htm)
- [Ctype.h kirjasto](https://www.tutorialspoint.com/c_standard_library/ctype_h.htm)