---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:56:33.582878-07:00
description: "Kuinka: C:ss\xE4 standardikirjaston funktiota `strlen()` k\xE4ytet\xE4\
  \xE4n yleisesti merkkijonon pituuden l\xF6yt\xE4miseen. T\xE4ss\xE4 on nopea esimerkki."
lastmod: '2024-03-13T22:44:57.029627-06:00'
model: gpt-4-0125-preview
summary: "C:ss\xE4 standardikirjaston funktiota `strlen()` k\xE4ytet\xE4\xE4n yleisesti\
  \ merkkijonon pituuden l\xF6yt\xE4miseen."
title: "Merkkijonon pituuden m\xE4\xE4ritt\xE4minen"
weight: 7
---

## Kuinka:
C:ssä standardikirjaston funktiota `strlen()` käytetään yleisesti merkkijonon pituuden löytämiseen. Tässä on nopea esimerkki:

```c
#include <stdio.h>
#include <string.h>

int main() {
    char myString[] = "Hello, World!";
    size_t length = strlen(myString);
    
    printf("Merkkijonon '%s' pituus on %zu.\n", myString, length);
    
    return 0;
}
```

**Esimerkkituloste:**
```
Merkkijonon 'Hello, World!' pituus on 13.
```

Tässä esimerkissä `strlen()` ottaa syötteenä merkkijonon (`myString`) ja palauttaa sen pituuden poislukien nolla-terminaattorin. Pituusmuuttujan käyttö `size_t` tyyppinä on suositeltavaa, koska se on etumerkkitön kokonaislukutyyppi, joka kykenee edustamaan järjestelmän suurimman mahdollisen objektin kokoa.

## Syväsukellus:
`strlen()`-funktio on ollut osa C-standardikirjastoa kielen syntymästä lähtien. Sisäisesti se toimii kasvattamalla laskuria edetessään merkkijonon läpi, kunnes se kohtaa nolla-terminaattorin. Tämän yksinkertaisuuden mukana tulee kuitenkin suorituskykyhuomioita: koska `strlen()` laskee merkkejä suoritusaikana, sen toistuva kutsuminen samalle merkkijonolle esimerkiksi silmukassa on tehottomuutta.

Turvallisuuden kannalta `strlen()` ja muut C-kielen merkkijonokäsittelyfunktiot eivät itsessään tarkista puskurin ylityksiä, mikä tekee huolellisesta ohjelmoinnista välttämätöntä haavoittuvuuksien välttämiseksi. Modernit vaihtoehdot muissa kielissä, kuten merkkijonotyypit, jotka sisältävät pituuden tai käyttävät oletuksena turvallista puskurin käsittelyä, poistavat joitakin näistä riskeistä ja tehottomuuksista.

Huolimatta sen rajoituksista, `strlen()`-funktion ja käsityöläismäisen merkkijonokäsittelyn ymmärtäminen C:ssä on tärkeää ohjelmoijille, erityisesti kun työskennellään matalan tason koodin kanssa tai kun suorituskyky ja muistinhallinta ovat ensiarvoisen tärkeitä. Se tarjoaa myös arvokkaita näkemyksiä muiden kielten korkeamman tason merkkijonoabstraktioihin.
