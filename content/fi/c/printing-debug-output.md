---
title:    "C: Virheenjäljitely tulostus"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Debug-tulostuksen tulostaminen on tärkeä osa C-ohjelmointia, joka auttaa ohjelmoijia tunnistamaan ja korjaamaan ohjelman virheitä ja ongelmia. Se on myös hyödyllistä, kun halutaan seurata ohjelman suorituksen kulkua ja tarkistaa, että ohjelma toimii odotetulla tavalla. Eli jos haluat olla tehokas C-ohjelmoija, sinun tulisi oppia tulostamaan debug-outputia.

## Miten

Tulostamisen perusmuoto C-kielellä on `printf()` -funktio. Se ottaa ensimmäisenä parametrinaan merkkijonon, joka määrittelee mitä ja miten tulostetaan. Seuraavat parametrit ovat muuttujia, jotka haluat tulostaa. Tässä on yksinkertainen esimerkki:

```C
#include <stdio.h>

int main()
{
    int x = 10;
    int y = 5;
    
    printf("x = %d, y = %d\n", x, y);
    
    return 0;
}

```

Tässä koodissa käytetään `%d` -konversiosymbolia, joka tarkoittaa, että ohjelma tulostaa arvot käyttäen kokonaislukumuotoa. Voit käyttää myös muita konversiosymboleja, kuten `%f` liukuluvuille, `%c` merkeille ja `%s` merkkijonoille. Lisäksi voit käyttää `\n` -merkkiä uuden rivin lisäämiseen.

## Syväsukellus

Tulostamisen lisäksi on olemassa muitakin tapoja käyttää debug-tulostusta, kuten virheilmoitusten ja varoitusten tulostaminen. Voit myös tulostaa tietoja muistiin varattujen alueiden sisällöstä ja ohjelman suorituksen aikana käytössä olevista muuttujista. Tämä voi auttaa tunnistamaan esimerkiksi muistin vuotokohtia ja muita suorituskykyongelmia.

Lisäksi on tärkeää huomata, että kun ohjelmasi on valmis ja toimii odotetulla tavalla, sinun tulisi poistaa kaikki debug-tulostukset. Tämä on tärkeää, jotta ohjelmasi suorituskyky ei kärsisi tarpeettomasta tulostamisesta.

## Katso myös

- [The Use of Debugging in C](https://www.programiz.com/c-programming/c-debugging)
- [Debugging and Diagnostics in C](https://www.tutorialspoint.com/cprogramming/c_debugging.htm)
- [Debugging in C: Tips and Tricks](https://www.cprogramming.com/debugging/debugging-tips.html)