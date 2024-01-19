---
title:                "Komentorivin argumenttien lukeminen"
html_title:           "Elm: Komentorivin argumenttien lukeminen"
simple_title:         "Komentorivin argumenttien lukeminen"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Komennon riviparametrien lukeminen on toimintatapa, jolla käyttäjän syötteitä voidaan lukea suoraan ohjelman käynnistyksen yhteydessä. Tätä tarvitaan usein, jolloin ohjelma voi käsitellä dynaamisia tietoja tai suorittaa erilaisia tehtäviä käyttäjän määritelmän mukaan.

## Näin se tehdään:

Tässä yksinkertainen esimerkki C-kielen koodista, jossa luetaan ja tulostetaan komennot riviparametrit.

```C 
#include <stdio.h>

int main(int argc, char *argv[]) {
    int i;
    for(i = 0; i < argc; i++) {
        printf("Argument %d: %s\n", i, argv[i]);
    }
    return 0;
}
```

Käynnistä ohjelma komennolla `./ohjelma arg1 arg2`. Ohjelman tuloste näyttää seuraavalta:

```
Argument 0: ./ohjelma
Argument 1: arg1
Argument 2: arg2
```

## Syvempi sukellus:

Komennon riviparametrien lukeminen on ollut osa C-kieltä sen alkuvuosista lähtien, tämän takia se on keskeinen osa ohjelmointikielen käsittelyä ja käyttöä. Vaihtoehtoja komentorivin parametrien lukemiseen ovat mm. stdio-kirjaston scanf-funktio tai tiedoston luku funktiot.

Riviparametrien lukemisessa käytettävät "argc" ja "argv" muuttujat ovat osa ohjelman "main" fuktiota. Argc kuvastaa argumenttien lukumäärää ja argv on osoitin merkkijonojen taulukkoon, jossa argit ovat.

## Katso myös:

Vaikka tämä artikkeli antaa yleiskuvan komentoriviparametrien lukemisesta, seuraavat lähteet tarjoavat syvempää tietoa ja konkreettisia esimerkkejä.

- C Programming Language, 2nd Edition by Brian W. Kernighan and Dennis M. Ritchie: https://www.amazon.com/Programming-Language-Brian-W-Kernighan/dp/0131103628
- Learn C the Hard Way by Zed Shaw: https://learncodethehardway.org/c/
- C tutorial by TutorialsPoint: https://www.tutorialspoint.com/cprogramming/index.htm