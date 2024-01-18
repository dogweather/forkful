---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Lua: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?

Regulaarilausekkeet ovat työkaluja, joita ohjelmoijat käyttävät löytääkseen halutunlaisia merkkijonoja tietystä tekstistä. Niitä käytetään esimerkiksi sähköpostiosoitteiden, puhelinnumeroiden tai verkkosivujen linkkien tunnistamiseen. Regulaarilausekkeiden avulla voidaan automaattisesti tarkistaa ja käsitellä tietoa ilman, että jokainen yksittäinen merkkijono täytyy tarkistaa käsin.

## Kuinka:

```
Lua 
local s = "Hello, world!"
local match = string.match -- Määritellään match-metodi sijoittamalla string.match muuttujaan
if match(s, "world") then -- Tarkistetaan, löytyykö merkkijonosta "world"
  print("Merkkijonosta löytyi sana \"world\"")
else
  print("Sanaa \"world\" ei löytynyt")
end
```

Tuloste: "Merkkijonosta löytyi sana "world""

## Syväsukellus:

Regulaarilausekkeiden historia voidaan jäljittää jo 1950-luvun alkuun asti, kun matemaatikko Stephen Kleene kehitti niitä käytettäväksi joukko-opin ja symbolisten koneiden kanssa. Nykyään niitä käytetään laajasti eri ohjelmointikielillä, kuten Lua, Python ja Java.

Vaikka regulaarilausekkeet ovat usein tehokas ratkaisu merkkijonojen käsittelyyn, on olemassa myös muita vaihtoehtoja, kuten parserit ja oliomallit. Regulaarilausekkeiden käytöstä voi myös aiheutua suorituskykyongelmia, jos niitä käytetään väärin tai liian monimutkaisilla säännöillä.

## Katso myös:

* [Lua-kirjaston string.match ohjeet](https://www.lua.org/pil/20.2.html)
* [Regulaarilausekkeiden opas](https://regexone.com/lesson/introduction_abcs)
* [Muita ohjelmointikieliä, joissa regulaarilausekkeita käytetään](https://en.wikipedia.org/wiki/Regular_expression#Programming_languages)