---
title:                "Väliaikaisen tiedoston luominen"
html_title:           "C++: Väliaikaisen tiedoston luominen"
simple_title:         "Väliaikaisen tiedoston luominen"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi

Joskus C++ -ohjelmointikielessä halutaan luoda väliaikainen tiedosto, joka sisältää väliaikaista tietoa tai muuta kaltaista dataa. Tämä voi olla hyödyllistä esimerkiksi, kun on tarvetta tallentaa jotain hetkellisesti ja poistaa se myöhemmin.

## Miten

C++ -kielen stdio.h -kirjaston avulla on mahdollista luoda väliaikainen tiedosto, joka tallentaa halutun datan. Alla olevassa koodiesimerkissä luodaan ensin väliaikainen tiedostonimi käyttämällä tmpnam()-funktiota ja sitten avataan tiedosto fopen()-funktion avulla. Tämä koodi tulee sijoittaa pääfunktioon, esimerkiksi main() -funktioon.

```C++
#include <iostream>
#include <stdio.h>

int main() {
  char filename[L_tmpnam];
  char *tmpname = tmpnam(filename);
  FILE *tmpfile = fopen(tmpname, "w+");
  fputs("Tämä on väliaikainen tiedosto!", tmpfile);
  fclose(tmpfile);
  return 0;
}
```

Kun ohjelma suoritetaan, se luo uuden väliaikaisen tiedoston ja tallentaa siihen tekstin "Tämä on väliaikainen tiedosto!". Tiedosto suljetaan sitten ja voidaan poistaa myöhemmin.

## Syventävä tieto

Väliaikaiset tiedostot luodaan usein käyttämällä tmpnam()-funktiota, joka luo uniikin nimen jokaiselle luodulle tiedostolle. On kuitenkin huomioitava, että tämä nimi saattaa olla altis tietoturvariskeille, koska se saattaa paljastaa tiedon muille käyttäjille. Tämän vuoksi on suositeltavaa käyttää mkstemp() -funktiota, joka luo tiedoston ja palauttaa salatun nimen.

See Also: [C++ stdio.h -kirjasto](https://www.cplusplus.com/reference/cstdio/) ja [luennoitsijan esimerkkikoodi](https://github.com/luennot-rautakarva/CPP2018/blob/master/Killajoe/TempfileExample.cpp)