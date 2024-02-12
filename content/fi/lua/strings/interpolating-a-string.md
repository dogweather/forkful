---
title:                "Merkkijonon interpolointi"
aliases:
- /fi/lua/interpolating-a-string/
date:                  2024-01-20T17:51:29.330391-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonon interpolointi"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? - Mikä ja miksi?
Merkkijonojen interpolaatio tarkoittaa muuttujien tai lausekkeiden arvojen "upottamista" merkkijonoihin. Koodaajat käyttävät sitä dynaamisten merkkijonojen luomiseen nopeasti ja helposti.

## How to: - Kuinka:
```Lua
-- Perus esimerkki
local name = "Mikko"
local age = 28
local greeting = string.format("Hei, nimeni on %s ja olen %d vuotta vanha.", name, age)
print(greeting)
-- Output: Hei, nimeni on Mikko ja olen 28 vuotta vanha.
```

```Lua
-- Käyttäen taulukoita
local person = {name = "Liisa", age = 31}
local intro = string.format("Tavataan %s, hän on %d vuotta.", person.name, person.age)
print(intro)
-- Output: Tavataan Liisa, hän on 31 vuotta.
```

```Lua
-- Käyttäen totuusarvoja ja ehtolauseita (ternary operator -tyyliin)
local skyIsBlue = true
local weather = string.format("Taivas on %s", skyIsBlue and 'sininen' or 'harmaa')
print(weather)
-- Output: Taivas on sininen
```

## Deep Dive - Syväsukellus
Aiemmin, ennen kuin `string.format` tuli kuvioihin, merkkijonojen yhdistäminen oli hankalampaa. Vanha koulu käytti konkatenointia (`..` operaattoria), mikä on edelleen kelvollinen mutta ei niin siisti:

```Lua
local status = "Saldosi on: " .. balance
```

`string.format` on vahva vaihtoehto, sillä se perustuu C-kielen `printf`-toiminnallisuuteen. Formaatin merkkijonot, kuten `%s` tekstille ja `%d` kokonaisluvuille, tekevät koodista luettavampaa. Lisäksi, numerot ja jopa kellonajat voidaan formatoida monipuolisesti.

Lua ei ole alun perin suunniteltu interpolointia varten, mutta `string.format` ja `..` operaattori ovat tulleet vakiokalustoon. Joissain modernissa skriptikielissä (esim. JavaScript tai Ruby), merkkijonointerpolointi on integroitu suoraan kieleen paljon yksinkertaisemmin syntakseilla, mutta Luassa se vaatii selkeämpiä askelia.

## See Also - Katso Lisää
- Lua 5.4 manual for `string.format`: https://www.lua.org/manual/5.4/manual.html#pdf-string.format
- Lua-users wiki page on Strings: http://lua-users.org/wiki/StringsTutorial
- 'Programming in Lua' for a deeper understanding of the language: https://www.lua.org/pil/
