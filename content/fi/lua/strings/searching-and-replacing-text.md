---
date: 2024-01-20 17:58:20.094070-07:00
description: "Etsim\xE4ll\xE4 ja korvaamalla teksti\xE4 voit muokata merkkijonoja\
  \ nopeasti. Koodareille t\xE4m\xE4 on keskeist\xE4, kun p\xE4ivitet\xE4\xE4n dataa\
  \ tai siistit\xE4\xE4n sy\xF6tett\xE4."
lastmod: '2024-03-13T22:44:56.683234-06:00'
model: gpt-4-1106-preview
summary: "Etsim\xE4ll\xE4 ja korvaamalla teksti\xE4 voit muokata merkkijonoja nopeasti.\
  \ Koodareille t\xE4m\xE4 on keskeist\xE4, kun p\xE4ivitet\xE4\xE4n dataa tai siistit\xE4\
  \xE4n sy\xF6tett\xE4."
title: Tekstin etsiminen ja korvaaminen
---

{{< edit_this_page >}}

## What & Why? - Mitä ja miksi?

Etsimällä ja korvaamalla tekstiä voit muokata merkkijonoja nopeasti. Koodareille tämä on keskeistä, kun päivitetään dataa tai siistitään syötettä.

## How to: - Kuinka:

```Lua
-- Yksinkertainen tekstinkorvaus
local original = "Hello, world!"
local replaced = original:gsub("world", "Lua")
print(replaced)  -- Output: Hello, Lua!

-- Malliesimerkki säännöllisten lausekkeiden käytöstä
local log = "Error: Line 1024 - Invalid syntax."
local corrected = log:gsub("(%d+)", function(number) 
    return tonumber(number) + 1 
end)
print(corrected)  -- Output: Error: Line 1025 - Invalid syntax.

-- Globaali korvaus (kaikki esiintymät)
local story = "The fox jumps over the lazy dog. The dog did not move."
local fixed_story = story:gsub("dog", "cat")
print(fixed_story)  -- Output: The fox jumps over the lazy cat. The cat did not move.
```

## Deep Dive - Syväsukellus

Lua-kielessä tekstinkorvaus tehdään `string.gsub` -funktiolla. `gsub` lyhenee "global substitution" eli globaalista korvauksesta. Se löi läpi versiossa 5, joka toi mukanaan tehokkaat kuviovasteet ja säännölliset lausekkeet.

Vaihtoehdot? Voit käyttää `string.find` etsiäksesi ennen korvausta tai koota monimutkaisempia käsittelyitä. Mutta monesti `gsub` riittää.

Mitä teknisempiin yksityiskohtiin tulee, `gsub` käyttää 'pattern matching' -tekniikkaa, joka on yksinkertaistettu säännöllisten lausekkeiden muoto. Huomaathan, ettei Lua tue täysiä säännöllisiä lausekkeita kuten jotkin muut kielet.

## See Also - Katso myös

- Lua 5.4 referenssidokumentaatio: `string.gsub`: https://www.lua.org/manual/5.4/manual.html#pdf-string.gsub
- Lua-users wiki, String Library Tutorial: http://lua-users.org/wiki/StringLibraryTutorial
- Lua-users wiki, Patterns Tutorial: http://lua-users.org/wiki/PatternsTutorial
