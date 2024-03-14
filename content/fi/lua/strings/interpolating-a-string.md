---
date: 2024-01-20 17:51:29.330391-07:00
description: "Merkkijonojen interpolaatio tarkoittaa muuttujien tai lausekkeiden arvojen\
  \ \"upottamista\" merkkijonoihin. Koodaajat k\xE4ytt\xE4v\xE4t sit\xE4 dynaamisten\u2026"
lastmod: '2024-03-13T22:44:56.684071-06:00'
model: gpt-4-1106-preview
summary: "Merkkijonojen interpolaatio tarkoittaa muuttujien tai lausekkeiden arvojen\
  \ \"upottamista\" merkkijonoihin. Koodaajat k\xE4ytt\xE4v\xE4t sit\xE4 dynaamisten\u2026"
title: Merkkijonon interpolointi
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
