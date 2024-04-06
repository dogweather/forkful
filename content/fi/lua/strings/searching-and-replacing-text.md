---
date: 2024-01-20 17:58:20.094070-07:00
description: "How to: - Kuinka: Lua-kieless\xE4 tekstinkorvaus tehd\xE4\xE4n `string.gsub`\
  \ -funktiolla. `gsub` lyhenee \"global substitution\" eli globaalista korvauksesta.\
  \ Se l\xF6i\u2026"
lastmod: '2024-04-05T22:38:57.293056-06:00'
model: gpt-4-1106-preview
summary: "- Kuinka: Lua-kieless\xE4 tekstinkorvaus tehd\xE4\xE4n `string.gsub` -funktiolla.\
  \ `gsub` lyhenee \"global substitution\" eli globaalista korvauksesta. Se l\xF6\
  i l\xE4pi versiossa 5, joka toi mukanaan tehokkaat kuviovasteet ja s\xE4\xE4nn\xF6\
  lliset lausekkeet. Vaihtoehdot? Voit k\xE4ytt\xE4\xE4 `string.find` etsi\xE4ksesi\
  \ ennen korvausta tai koota monimutkaisempia k\xE4sittelyit\xE4. Mutta monesti `gsub`\
  \ riitt\xE4\xE4. Mit\xE4 teknisempiin yksityiskohtiin tulee, `gsub` k\xE4ytt\xE4\
  \xE4 'pattern matching' -tekniikkaa, joka on yksinkertaistettu s\xE4\xE4nn\xF6llisten\
  \ lausekkeiden muoto. Huomaathan, ettei Lua tue t\xE4ysi\xE4 s\xE4\xE4nn\xF6llisi\xE4\
  \ lausekkeita kuten jotkin muut kielet."
title: Tekstin etsiminen ja korvaaminen
weight: 10
---

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
