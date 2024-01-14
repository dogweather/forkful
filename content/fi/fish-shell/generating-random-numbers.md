---
title:                "Fish Shell: Satunnaislukujen luominen"
programming_language: "Fish Shell"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Satunnaisluvun generointi on tärkeä osa ohjelmointia. Se mahdollistaa monipuolisen ja vaihtelevan datajoukon luomisen, jota voi hyödyntää erilaisissa sovelluksissa ja algoritmeissa. Lisäksi se voi auttaa ohjelmoijaa luomaan testidataa kehittämässä uusia ominaisuuksia tai uudistamaan vanhoja.

## Miten tehdä

Fish Shell tarjoaa kätevän tavan generoida satunnaisia lukuja käyttäen `seq`-komennon `-r`-valitsinta. Se generoi halutun määrän satunnaisia lukuja tietyllä välillä. Esimerkiksi seuraava komento generoi 10 satunnaista kokonaislukua väliltä 1-100.

```Fish Shell
seq -r 1 100 | head -n 10
```
Tulostus näyttää tältä:

```Fish Shell
58
23
89
12
45
7
91
34
78
50
```

Voit myös käyttää `shuf`-komentoa luomaan satunnaisia permutaatioita. Seuraava esimerkki osoittaa kuinka voit generoida satunnaisen järjestyksen numerosarjoille 1-5.

```Fish Shell
seq 1 5 | shuf
```
Tulostus voisi näyttää esimerkiksi tältä:

```Fish Shell
3
1
5
4
2
```

Voit myös käyttää `($RANDOM)`-komennon asettamaan satunnaisen numeroarvon muuttujaan ja sitten käyttää sitä jossain toisessa komennossa. Esimerkiksi voit luoda skriptin, joka arpoo satunnaisen numeron ja lisää sen tiedoston nimeen.

```Fish Shell
touch "uusi_tiedosto($RANDOM)"
```
Tämä luo uuden tiedoston nimellä "uusi_tiedosto" ja siihen lisätään satunnainen numero. Näin voit luoda isoja määriä samanlaisia mutta eri nimisiä tiedostoja.

## Syvempi sukellus

Satunnaislukujen generoiminen perustuu algoritmeihin, jotka luovat lukuja käyttämällä tiettyjä sääntöjä. Fish Shell käyttää `arc4random`-algoritmia, joka pohjautuu RC4-salausalgoritmiin. Tämä algoritmi luo satunnaisia lukuja käyttämällä salauksessa tarvittavia satunnaisia lukuja. Se on kehitetty tarjoamaan parempaa suorituskykyä ja turvallisuutta verrattuna muihin satunnaislukualgoritmeihin.

On myös tärkeää muistaa, että satunnaislukujen generoiminen voi olla haastavaa tietokoneelle, joten täysin satunnaisia lukuja ei oikeastaan voi luoda. Usein käytetäänkin algoritmeja, jotka pyrkivät tuottamaan mahdollisimman sattumanvaraisia lukuja.

## Katso myös

- [Fish Shellin dokumentaatio satunnaisluvuista](https://fishshell.com/docs/current/cmds/seq.html)
- [Shuf-komennon dokumentaatio](https://man.archlinux.org/man/core/coreutils/shuf.1.en)
- [Artikkeli siitä, kuinka satunnaislukuja generoidaan](https://www.pcworld.com/article/2883932/how-to-generate-truly-random-numbers-with-your-pc.html)