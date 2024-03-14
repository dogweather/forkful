---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:05:24.992621-07:00
description: "Debug-tulosteen tulostaminen tarkoittaa v\xE4liaikaisten, informatiivisten\
  \ lokiviestien tuottamista, jotka voivat auttaa ohjelmoijia ymm\xE4rt\xE4m\xE4\xE4\
  n ohjelman\u2026"
lastmod: '2024-03-13T22:44:57.043249-06:00'
model: gpt-4-0125-preview
summary: "Debug-tulosteen tulostaminen tarkoittaa v\xE4liaikaisten, informatiivisten\
  \ lokiviestien tuottamista, jotka voivat auttaa ohjelmoijia ymm\xE4rt\xE4m\xE4\xE4\
  n ohjelman\u2026"
title: "Tulostetaan virheenj\xE4ljitystietoja"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Debug-tulosteen tulostaminen tarkoittaa väliaikaisten, informatiivisten lokiviestien tuottamista, jotka voivat auttaa ohjelmoijia ymmärtämään ohjelman kulkua ja tilaa sen suorituksen aikana. Ohjelmoijat tekevät tätä tunnistaakseen ja diagnosoidakseen ohjelmistobugeja tai odottamattomia käyttäytymisiä ohjelman logiikassa.

## Kuinka:

C:ssä yleisin tapa tulostaa debug-tuloste on käyttämällä `printf`-funktiota standardin I/O-kirjastosta. `printf`-funktio mahdollistaa muotoillun tulosteen standardilähtölaitteelle, tyypillisesti näytölle. Tässä on yksinkertainen esimerkki:

```c
#include <stdio.h>

int main() {
    int x = 5;
    printf("Debug: x:n arvo on %d\n", x);
    
    // Ohjelmasi logiikka täällä
    
    return 0;
}
```

Esimerkkituloste:

```
Debug: x:n arvo on 5
```

Monimutkaisempaa debug-tulostusta varten saatat haluta sisällyttää tiedostonimen ja rivinumeron tiedot. Tämä voidaan tehdä käyttämällä `__FILE__` ja `__LINE__` esimääriteltyjä makroja näin:

```c
#define DEBUG_PRINT(fmt, args...) fprintf(stderr, "DEBUG: %s:%d: " fmt, __FILE__, __LINE__, ##args)

int main() {
    int testiarvo = 10;
    DEBUG_PRINT("Testiarvo on %d\n", testiarvo);
    
    // Ohjelmasi logiikka täällä
    
    return 0;
}
```

Esimerkkituloste:

```
DEBUG: esimerkki.c:6: Testiarvo on 10
```

Huomaa, että tässä esimerkissä käytämme `fprintf`:ää tulostamaan standardivirhevirtaan (`stderr`), mikä on usein sopivampi debug-viesteille.

## Syväluotaus

Historiallisesti C:n debuggaustekniikat ovat olleet manuaalisia ja alkeellisia johtuen kielenclose-to-the-metal-filosofiasta ja iästä. Kun taas modernit kielet saattavat sisältää monimutkaisia, sisäänrakennettuja debuggauskirjastoja tai luottaa vahvasti Integroituun Kehitysympäristöön (IDE) -ominaisuuksiin, C-ohjelmoijat turvautuvat usein manuaalisesti lisäämään tulostuslauseita, kuten yllä on esitetty, jäljittääkseen ohjelmansa suorituksen.

Yksi asia, josta debug-tulosteiden kanssa tulee varoa, on niiden potentiaali sotkea tulostetta ja johtaa suorituskykyongelmiin, erityisesti jos ne jätetään tahattomasti tuotantokoodiin. Näistä syistä ehdollinen käännös (esim., `#ifdef DEBUG ... #endif`) saattaa olla parempi lähestymistapa, mahdollistaen debug-lauseiden sisällyttämisen tai poissulkemisen käännösaikaisilla lipuilla.

Lisäksi nyt on saatavilla kehittyneempiä työkaluja ja kirjastoja C:n debuggaamiseen, kuten GDB (GNU Debugger) ja Valgrind muistivuotojen havaitsemiseen. Nämä työkalut tarjoavat integroidumman lähestymistavan debuggaamiseen ilman tarvetta muokata koodia lisäämällä tulostuslauseita.

Kuitenkin `printf`:n debuggauksen yksinkertaisuus ja välitön palaute eivät ole vähäpätöisiä, tehden siitä hyödyllisen työkalun ohjelmoijan työkalupakkiin, erityisesti niille, jotka vasta opettelevat C:n monimutkaisuuksia.
