---
title:                "C: Lukemassa komentoriviparametreja"
simple_title:         "Lukemassa komentoriviparametreja"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

Ohjelmointi on monipuolista ja mielenkiintoista, ja osa siitä on käsitellä käyttäjän antamia syötteitä. Komentoriviparametrit ovat tärkeitä tällä alueella, ja tämä blogipostaus auttaa sinua ymmärtämään, miten niitä käsitellään C-kielellä.

## Miten tehdä

Komentoriviparametrien lukeminen C-kielellä on helppoa, kun tiedät mitä teet. Alla olevassa koodiesimerkissä näet yksinkertaisen tavan lukea komentoriviltä annettua argumenttia ja tulostaa se konsoliin:

```C
#include <stdio.h>

int main(int argc, char *argv[])
{
    if (argc > 1)
    {
        printf("Ensimmäinen komentoriviparametri on: %s\n", argv[1]);
    }
    else
    {
        printf("Et antanut komentoriviparametreja.\n");
    }
    return 0;
}
```

Kun ajat tämän koodin komentoriviltä ja annat sille esimerkiksi seuraavanlaisen syötteen: `./ohjelma "Tervetuloa"`, näet tuloksen `Ensimmäinen komentoriviparametri on: Tervetuloa`. Huomaat, että voit käyttää `argv`-muuttujaa saadaksesi pääsyn komentoriviparametreihin. Voit myös käyttää `argc`-muuttujaa tarkistaaksesi, onko käyttäjä antanut syötettä vai ei.

## Syvemmälle

C-kielellä on muitakin tapoja käsitellä komentoriviparametreja. Voit esimerkiksi käyttää `getopt`-funktiota, joka auttaa sinua lukemaan ja käsittelemään komentoriviparametreja. Voit myös käyttää `scanf`-funktiota lukemaan merkkijonoja suoraan komentoriviltä.

Jokainen käyttötilanne on erilainen, joten on tärkeää tutustua eri tapoihin lukea komentoriviparametreja ja valita se, joka sopii parhaiten tarpeisiisi.

## Katso myös

- [C Cheat Sheet](https://www.codecademy.com/learn/learn-c/modules/learn-c-cpp-cheatsheet)
- [C-kurssi Codecademyssa](https://www.codecademy.com/learn/learn-c)
- [getopt-dokumentaatio](https://www.gnu.org/software/libc/manual/html_node/Example-of-Getopt.html)