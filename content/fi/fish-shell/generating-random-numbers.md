---
title:                "Sattumanvaraisten numeroiden luominen"
html_title:           "Fish Shell: Sattumanvaraisten numeroiden luominen"
simple_title:         "Sattumanvaraisten numeroiden luominen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Satunnaisten numeroiden luominen on yksi yleisistä ohjelmointitehtävistä, jota käytetään monissa erilaisissa sovelluksissa. Se on tapa saada tietokone generoimaan satunnaisia lukuja, jotka voidaan sitten käyttää esimerkiksi arpajaisissa, pelien toiminnassa tai salausavaimissa.

## Miten?

Fish Shell tarjoaa helpon tavan generoida satunnaisia lukuja. Käyttämällä `rand` -komentoa, voit luoda haluamasi määrän satunnaisia lukuja halutussa välissä. Esimerkiksi `rand 1 10` generoi yhden satunnaisen luvun välillä 1-10.

```Fish Shell
> rand 1 10
5
```

Voit myös antaa `rand` -komennolle parametrina halutun lukumäärän ja se generoi automaattisesti satunnaiset luvut välillä 0-100.

```Fish Shell
> rand 5
12 45 76 23 99
```

## Syvempi sukellus

Satunnaisia lukuja on käytetty jo varhaisista tietokoneaikakausista lähtien. Aluksi niitä generoitiin fyysisillä menetelmillä, kuten esimerkiksi arpakuution heittämisellä. Nykyään tietokoneet käyttävät monimutkaisempia algoritmeja luomaan satunnaisia lukuja, jotka ovat mahdollisimman sattumanvaraisia.

Toinen tapa generoida satunnaisia lukuja Fish Shellissä on käyttämällä `openssl` -komentoa. Tämä tarjoaa enemmän vaihtoehtoja, kuten mahdollisuuden generoida myös satunnaisia salasanoja.

```Fish Shell
> openssl rand -base64 10
DQ+jG+4hQq8UlA==
```

## Tutustu myös

- [Fish Shell dokumentaatio](https://fishshell.com/docs/current/index.html)
- [Fish Shellin `rand` -komento](https://fishshell.com/docs/current/cmds/rand.html)
- [Wikipedia-artikkeli satunnaisluvuista](https://en.wikipedia.org/wiki/Random_number_generation)