---
title:                "Testien kirjoittaminen"
html_title:           "C: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Testien kirjoittaminen on tärkeä osa ohjelmointia, koska se auttaa varmistamaan, että koodi toimii odotetulla tavalla ja estää mahdollisten bugien pääsyn tuotantoympäristöön.

## Miten aloittaa

Testien kirjoittaminen C-kielellä on yksinkertaista. Alla on esimerkki yksinkertaisesta funktiosta, joka tarkistaa, onko annettu luku parillinen vai ei:

```C
#include <stdio.h>

int main()
{
    int num = 6;
    if(num % 2 == 0)
    {
        printf("Luku %d on parillinen", num);
    }
    else
    {
        printf("Luku %d on pariton", num);
    }

    return 0;
}
```
Tämän koodin tuloste olisi "Luku 6 on parillinen". Voit myös muokata tätä esimerkkiä omien funktioidesi testaamiseen.

## Syvempi sukellus
Testien kirjoittamisessa on tärkeää muistaa, että niiden on oltava yksinkertaisia ja selkeitä. Testien tarkoituksena on auttaa sinua vahvistamaan, että koodisi toimii odotetulla tavalla eikä löydä virheitä. Hyödyt testien kirjoittamisesta ovat suurempia kuin mahdolliset haitat, kuten lisätyö ja aika. Muista myös käyttää hyviä testaustekniikoita, kuten yksikkötestausta ja jatkuvaa integrointia, parhaan mahdollisen tuloksen saavuttamiseksi.

## Katso myös
- [C-kielen virallinen dokumentaatio](https://en.cppreference.com/w/c)
- [Testien kirjoittaminen C-kielellä käyttäen Unity-testikirjastoa](https://medium.com/@rushman/test-driven-development-in-c-with-the-unity-test-framework-832d3cd0e107)
- [Jatkuvan integroinnin perusteet C-projekteissa](https://medium.com/@alexandrechoisy/getting-started-with-continuous-integration-for-a-c-project-887a1758badc)