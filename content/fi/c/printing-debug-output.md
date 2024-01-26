---
title:                "Virheenjäljitystulosteiden tulostaminen"
date:                  2024-01-20T17:51:56.000464-07:00
model:                 gpt-4-1106-preview
simple_title:         "Virheenjäljitystulosteiden tulostaminen"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? / Mitä & Miksi?

Koodia kirjoittaessa debug-tulostus auttaa virheiden jäljittämisessä ilmoittamalla ohjelman tilasta. Se on kehittäjän työkalu ymmärtää, missä ja miksi koodi epäonnistuu tai käyttäytyy odottamattomasti.

## How to: / Kuinka:

```c
#include <stdio.h>

int main() {
    int testVariable = 5;
    printf("Debug: testVariable arvo on %d\n", testVariable);
    // Do something with testVariable
    return 0;
}
```

Tulostaisi: `Debug: testVariable arvo on 5`

```c
// Monimutkaisemmassa skenaariossa voit käyttää makroa helpottamaan:
#include <stdio.h>

#define DEBUG_PRINT(fmt, ...) fprintf(stderr, "DEBUG: %s:%d:%s(): " fmt, \
    __FILE__, __LINE__, __func__, __VA_ARGS__)

int main() {
    int testVariable = 5;
    DEBUG_PRINT("testVariable arvo on %d\n", testVariable);
    // Do something with testVariable
    return 0;
}
```

Tulostaisi esim.: `DEBUG: example.c:10:main(): testVariable arvo on 5`

## Deep Dive / Syväsukellus:

Debug-tulostuksen konsepti on vanha kuin itse ohjelmointi. Virheenjäljitys on aina ollut osa ohjelmistokehitystä. Alkuaikoina ohjelmoijat saattoivat lukea reikänauhoja tai tarkkailla valoja ja katkaisijoita. Nykyisin voimme valita monista välineistä ja tekniikoista. Esimerkiksi `printf`-tyyppiset funktiot C-kielessä tai rikkaammat työkalut kuten GDB.

`printf`-debugging on yksinkertaista ja toimii melkein missä tahansa ympäristössä. Alternatiiveina ovat viralliset debuggerit tai kirjastot, jotka tarjoavat loggausominaisuuksia, kuten syslog Linux-ympäristössä.

Toteutuksen yksityiskohdissa merkittävää on muistaa puhtaus ja selkeys koodissa. Liian monta debug-tulostusta voi sotkea koodin ymmärtämisen ja virheenjäljityksen. Kun virheet on löydetty ja korjattu, on hyvä poistaa tai kommentoida tarpeettomat debug-tulosteet.

## See Also / Lue Lisää:

- C Standard Library documentation: https://en.cppreference.com/w/c
- GNU Debugger (GDB): https://www.gnu.org/software/gdb/
- Effective debugging strategies: https://www.cs.swarthmore.edu/~newhall/unixhelp/howto_gdb.php
