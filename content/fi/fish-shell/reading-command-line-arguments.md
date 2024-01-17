---
title:                "Komentoriviparametrien lukeminen"
html_title:           "Fish Shell: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Komentoriviparametrien lukeminen, tai "reading command line arguments", on tapa saada ohjelmalle annettuja syötteitä suoraan komentoriviltä. Tämä on tärkeää, sillä se mahdollistaa ohjelman joustavan käytön ja mahdollistaa sen mukauttamisen erilaisiin tarpeisiin.

## Miten:
### Esimerkki 1:
```Fish Shell``` käyttää ```$argv``` muuttujaa komentoriviparametrien lukemiseen. Voit tulostaa kaikki annetut parametrit käyttämällä seuraavaa komentoa:
```
echo $argv
```
Tulostuksena näet listan komentoriviparametreista. Esimerkiksi jos syötit komennon ```fish omat_komennot.fish 123 abc```, tulostuksena näkyy ```123 abc```.

### Esimerkki 2:
Voit myös käyttää ```$argv```:n indeksointia, jolloin voit valita tietyn parametrin tulostettavaksi. Esimerkiksi komento ```echo $argv[1]``` tulostaa ```123```.

## Syväsukellus:
Komennon riviparametrien lukeminen on ollut osa käyttöjärjestelmien toimintaa jo pitkään. Aiemmin tätä tehtiin usein C-kielellä, mutta nykyisin erilaiset kielivalinnat, kuten Fish Shell, tarjoavat helpomman tavan lukea komentoriviparametreja.

Vaihtoehtoisia tapoja käsitellä komentoriviparametreja on esimerkiksi käyttää erillisiä kirjastoja kuten ```getopt```, joka tarjoaa mahdollisuuden helpompaan parametrien käsittelyyn.

Fish Shellissä ```$argv``` muuttuja on käytettävissä ohjelman käynnistyksen alusta asti, mikä mahdollistaa komentoriviparametrien käsittelyn myös varhaisessa vaiheessa.

## Katso myös:
- [Fish Shellin dokumentaatio](https://fishshell.com/docs/current/index.html)
- [getopt kirjaston dokumentaatio](https://www.gnu.org/software/libc/manual/html_node/Getopt.html)