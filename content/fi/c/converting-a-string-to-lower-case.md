---
title:                "C: Muuttaminen merkkijonoksi pienaakkosiksi"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Monissa tilanteissa on tarpeellista muuntaa merkkijono pienaakkosiksi, esimerkiksi kun halutaan vertailla merkkijonoja tai tehdä niistä tulosteita. Tämä blogikirjoitus käsittelee kuinka muuntaa merkkijono pienaakkosiksi C-ohjelmoinnissa.

## Kuinka

Merkkijonon muuntaminen pienaakkosiksi C:ssä on helppoa. Ensimmäinen vaihe on määrittää merkkijono, joka halutaan muuntaa. Tämän jälkeen käytetään C-ohjelmoinnissa valmiiksi määriteltyä funktiota "tolower()", ja syötetään siihen muunnettava merkkijono. Koodin tulisi näyttää suunnilleen tältä:

```
#include <stdio.h>
#include <string.h>

int main()
{
  char str[] = "TÄMÄ ON MERKKIJONO";
  printf("Ennen muuntamista: %s\n", str);
  
  // Käytetään "tolower()" funktiota muuntaaksemme merkkijonon pienaakkosiksi
  for(int i = 0; str[i]; i++)
  {
    str[i] = tolower(str[i]);
  }

  printf("Pienaakkosiksi muunnettuna: %s\n", str);
  return 0;
}
```

Tuloste:

```
Ennen muuntamista: TÄMÄ ON MERKKIJONO
Pienaakkosiksi muunnettuna: tämä on merkkijono
```

## Syvemmälle

Miksi käytämme "tolower()" funktiota? Se johtuu siitä, että C-ohjelmointikielessä kirjaimet tallennetaan ASCII-numerokoodimuodossa, ja suurilla ja pienillä kirjaimilla on erilaiset koodiarvot. Muuttamalla kirjaimen ASCII-koodiarvoa pienimmästä suurimpaan saamme kirjaimen pienaakkoseksi.

On myös tärkeää huomata, että "tolower()" funktio muuntaa merkkijonon vain sisäisesti, mutta ei tallenna sitä vakioksi. Jos haluamme muuttaa alkuperäisen merkkijonon pysyvästi, voimme käyttää esimerkiksi "strcpy()" funktiota kopioidaksemme pienaakkosmuotoisen merkkijonon uuteen muuttujaan.

## Katso myös

- [tolower() funktio C:ssä](https://www.tutorialspoint.com/c_standard_library/c_function_tolower.htm)
- [strcpy() funktio C:ssä](https://www.tutorialspoint.com/c_standard_library/c_function_strcpy.htm)