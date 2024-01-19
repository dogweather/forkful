---
title:                "Merkkijonon pituuden selvittäminen"
html_title:           "Go: Merkkijonon pituuden selvittäminen"
simple_title:         "Merkkijonon pituuden selvittäminen"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Merkkijonon pituuden etsiminen on toimenpide, jolla selvitetään kuinka monta merkkiä merkkijonossa on. Ohjelmoijat tekevät tämän esimerkiksi silloin, kun heidän tarvitsee rajoittaa merkkijonon pituutta tai jakaa se osiin.

## Miten:

Katsotaanpa, kuinka se toimii suoraan esimerkkiä käyttäen:

```Lua 
local str = "Hei, Suomi!"
print(#str)
```

Se tulostaa:

```Lua 
12
```

Tämä Lua-koodi laskee merkkijono "Hei, Suomi!" merkkien määrän ja tulostaa sen.

## Syväsukellus:

(1) Historiallinen asiayhteys: Lua on ohjelmointikieli, joka on suunniteltu erityisesti upotettaviin sovelluksiin. Se on yksinkertainen ja tehokas, ja sen sisäänrakennetut työkalut, kuten merkkijonon pituuden laskeminen, tekevät siitä sopivan valinnan moniin tehtäviin.

(2) Vaihtoehtoja: On muitakin tapoja laskea merkkijonon pituus Luassa. Yksi näistä on 'string.len' funktio. Esimerkki:

```Lua
local str = "Hei, Suomi!"
print(string.len(str))
```

(3) Toteutuskohdat: '#'-operaattori Luassa on kätevä työkalu, mutta sillä on myös rajoituksia. Se ei esimerkiksi toimi kunnolla, jos merkkijonossa on nolla tavua ('\0'). Tässä tapauksessa 'string.len' -funktio on parempi valinta, koska se laskee kaikki tavut.

## Katso myös:

Koodin viite: [Lua 5.4 merkkijono-operaattoridokumentaatio](http://www.lua.org/manual/5.4/manual.html#3.4.7)

Ohjekirja: [Programming in Lua](https://www.lua.org/pil/)

Luiz Henrique de Figueiredonin sivu: [Lua-ohjelmointikielen tie operaattorien luomiseen](http://lua-users.org/wiki/User:LuizHenriqueDeFigueiredo)