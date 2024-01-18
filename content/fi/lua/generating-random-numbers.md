---
title:                "Satunnaisten lukujen generointi"
html_title:           "Lua: Satunnaisten lukujen generointi"
simple_title:         "Satunnaisten lukujen generointi"
programming_language: "Lua"
category:             "Lua"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Satunnaisten numeroiden luominen on tärkeä osa monien ohjelmien toimintaa. Satunnaiset numerot ovat lukuja, jotka eivät seuraa mitään tiettyä kaavaa, ja niitä voidaan käyttää moniin eri tarkoituksiin ohjelmoinnissa. Tämän vuoksi ohjelmoijat käyttävät satunnaislukugeneraattoreita varmistaakseen, että ohjelma toimii joka kerta eri tavalla.

## Miten:
Random-numeroiden generoiminen Lua-kielellä on helppoa ja nopeaa. Käytä funktiota `math.random()`, joka voi ottaa joko yhden tai kaksi parametria. Ensimmäinen parametri määrittää pienimmän mahdollisen arvon ja toinen parametri suurimman mahdollisen arvon, esimerkiksi:
```
Lua
math.random(1,5) -- palauttaa satunnaisen luvun väliltä 1-5
math.random() -- palauttaa satunnaisen luvun väliltä 0-1
```
Voit myös asettaa vakioksi käytettävän satunnaislukugeneraattorin käyttämällä funktiota `math.randomseed()`, esimerkiksi:
```
Lua
math.randomseed(123) -- käyttää satunnaisten lukujen luomiseen samaa järjestystä joka kerta
```

## Syvemmälle:
Satunnaisten lukujen generointi on ollut tärkeä osa ohjelmointia jo vuosikymmenien ajan. Alun perin tietokoneet eivät pystyneet luomaan oikeasti sattumanvaraisia numeroita vaan tietokoneen aika ja käyttäjän antamat syötteet käytettiin apuna satunnaisuutta luodessa. Nykyään tietokoneet voivat generoida täysin sattumanvaraisia numeroita, mutta joissain tapauksissa voidaan silti käyttää muita menetelmiä, kuten satunnaisten lukujen generoimista ulkopuolisista lähteistä.

## Katso myös:
Voit löytää lisää tietoa satunnaislukujen generoimisesta Lua-kielellä LuaWikista: [LuaWiki - Satunnaiset numerot](https://lua.wikia.org/wiki/Math.random)