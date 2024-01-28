---
title:                "Interaktiivisen komentotulkin (REPL) käyttö"
date:                  2024-01-26T04:11:34.551212-07:00
model:                 gpt-4-0125-preview
simple_title:         "Interaktiivisen komentotulkin (REPL) käyttö"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Interaktiivinen kuori, tai Lue-Arvo-Tulosta-Silmukka (REPL), on työkalu, joka tarjoaa reaaliaikaisen koodausympäristön koodinpätkien välittömään testaamiseen. Ohjelmoijat käyttävät sitä nopean palautteen saamiseen kehityksen, oppimisen ja virheenkorjauksen aikana.

## Kuinka:
C ei sisällä sisäänrakennettua REPL:iä, mutta voit käyttää kolmannen osapuolen työkaluja. Tässä vilkaisu Clingiin, C++-tulkkiiin, joka pystyy käsittelemään myös C-koodia:

```C
#include <stdio.h>

int main() {
    printf("Hello, REPL world!\n");
    return 0;
}
```

Tuloste Cling REPL:ssä:
```
[cling]$ .x yourscript.c
Hello, REPL world!
```

Cling suorittaa skriptin ja tulostaa tuloksen välittömästi.

## Syväsukellus
REPL:t ovat vakioita dynaamisissa kielissä kuten Python tai Ruby, mutta käännetyille kielille kuten C, ne ovat harvinaisempia. Historiallisesti käännä-suorita-virheenkorjaus-sykli ei soveltunut interaktiiviseen tutkimiseen. Työkalut kuten Cling ja online C-kääntäjät tarjoavat REPL:n kaltaisia kokemuksia käärimällä C-koodisi C++-ympäristöön.

Vaihtoehtoja Clingille ovat C-tulkit kuten CINT ja Ch. Nämä työkalut mahdollistavat nopean iteraation, mutta eivät ehkä sovellu kaikkiin kehitysskenaarioihin suorituskykyrajoitteiden ja monimutkaisten ominaisuuksien tukemisen vuoksi.

REPL:n toteutus käännetyssä kielessä sisältää koodinpätkien kokoamisen ja suorittamisen lennosta, mikä ei ole triviaalia ja saattaa olla rajoitteita verrattuna koko kielen mahdollisuuksiin.

## Katso myös
- Cling: https://github.com/root-project/cling
- Online C Kääntäjä ja REPL: https://repl.it/languages/c
- CINT: http://root.cern.ch/drupal/content/cint
- Ch Tulkki: http://www.softintegration.com/products/chstandard/
