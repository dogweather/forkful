---
title:                "Kaavana vastaavien merkkien poistaminen"
html_title:           "Lua: Kaavana vastaavien merkkien poistaminen"
simple_title:         "Kaavana vastaavien merkkien poistaminen"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Poistaminen merkkejä vastaava kuvio on prosessi, jossa ohjelmoijat poistavat merkkejä tietystä kohdasta tai osasta merkkijonoa. Tämä voi olla hyödyllistä esimerkiksi tietyn tietokantaoperaation suorittamisessa tai tiettyjen merkkien filtteröimisessä.

## Miten:

```Lua
local string = "Tervetuloa Luaan!"
string = string.gsub("a", "")
print(string) -- Tulostaa: Tervetulou Lun!
```
```Lua
local string = "01020304050"
string = string:gsub("%d%d%d", "")
print(string) -- Tulostaa: 04050
```

## Deep Dive:

Poistaminen merkkejä vastaavan kuvion käsite on ollut käytössä jo pitkään, ja sitä käyttävät monet ohjelmointikielit kuten Perl ja Ruby. Lua tarjoaa myös samanlaisen toiminnallisuuden, joka on hyödyllinen monissa ohjelmointitehtävissä.

Toinen vaihtoehto tällaiseen poistamiseen on käyttää säännöllisiä lausekkeita (regular expressions), jotka tarjoavat monimutkaisemman tavan työskennellä merkkijonojen kanssa ja etsiä tiettyjä kuvioita. Lua:ssa on myös mahdollista käyttää säännöllisiä lausekkeita poistamiseen merkkejä vastaavan kuvion sijaan.

Lua:ssa poistaminen merkkejä vastaavan kuvion tekee ```gsub``` (global substitution) funktio, joka korvaa kaikki osuvat kuviot tietyn kuvion kanssa. Tämä toiminto tarjoaa myös mahdollisuuden valita, kuinka monta kuvioita korvataan, mikä tekee siitä erittäin joustavan työkalun.

## Katso myös:

- [Lua: gsub() Funktio](https://www.lua.org/manual/5.3/manual.html#pdf-string.gsub)
- [Regular Expressions Cheatsheet](https://www.debuggex.com/cheatsheet/regex/javascript)
- [Lua: string.match() Funktio](https://www.lua.org/manual/5.3/manual.html#pdf-string.match)