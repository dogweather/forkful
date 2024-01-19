---
title:                "Satunnaisten numeroiden luominen"
html_title:           "Bash: Satunnaisten numeroiden luominen"
simple_title:         "Satunnaisten numeroiden luominen"
programming_language: "Lua"
category:             "Lua"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Satunnaislukujen luominen on prosessi, jossa erilaisia numeroita luodaan ilman näkyvää kaavaa. Ohjelmoijat tekevät tämän usein simuloidakseen satunnaisuutta tai testatakseen ohjelmiensa suorituskykyä.

## Miten toimii:

Lua-toteutuksessa satunnaislukujen luominen on melko yksinkertaista käyttämällä 'math.random' -funktiota:

```Lua
-- Alustaa satunnaislukugeneraattorin
math.randomseed(os.time())

-- Tuottaa satunnaisen kokonaisluvun väliltä 1-100
randomNumber = math.random(100)
print(randomNumber)
```

Metodi 'math.random()' palauttaa satunnaisen luvun välillä 0 ja 1. Se voi myös palauttaa kokonaisluvun tietyn alueen sisällä.

## Syvempi tieto:

Historiallisessa kontekstissa satunnaislukujen generointi on ollut välttämätöntä monien perusohjelmistojen, kuten simulaatioiden ja tietokonepelien, toiminnalle. Lua tukee tätä perustoimintoa 'math.random' -toiminnolla.

Satunnaislukugeneraattorin vaihtoehtoja on useita, kuten Mersenne Twister tai Xorshift, mutta Lua käyttää C-kielisen tasoista rand()-funktiota, joka pohjautuu lineaariseen kongruenssialgoritmiin.

Yksityiskohdat toteutuksesta: 'math.randomseed' -funktiota tulisi kutsua kerran ohjelman alkaessa ja 'math.random' -funktiota käytetään lukujen tuottamiseen. Todellista satunnaisuutta ei voida saavuttaa, mutta tämä saa aikaan riittävän "epädeterministisen" tuloksen.

## Katso myös:

1. Lua math.random dokumentaatio: [Link](https://www.lua.org/manual/5.4/manual.html#6.7)
2. Mersenne Twister Wikipedia: [Link](https://fi.wikipedia.org/wiki/Mersenne_twister)
3. Xorshift Wikipedia: [Link](https://fi.wikipedia.org/wiki/Xorshift)