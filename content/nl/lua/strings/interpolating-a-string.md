---
title:                "Een string interpoleren"
aliases:
- /nl/lua/interpolating-a-string.md
date:                  2024-01-28T22:01:53.610232-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een string interpoleren"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/lua/interpolating-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Stringinterpolatie stelt je in staat om variabelen direct in te voegen in strings. Dit wordt gedaan om dynamisch strings op te bouwen en de code schoon te houden.

## Hoe:
In Lua, gebruik `..` voor concatenatie of `string.format` voor interpolatie. Voorbeeld:
```Lua
local name = "Ada"
local greeting = "Hallo, " .. name .. "!"
print(greeting) -- Uitvoer: Hallo, Ada!

local age = 30
local bio = string.format("%s is %d jaar oud.", name, age)
print(bio) -- Uitvoer: Ada is 30 jaar oud.
```

## Diepgaand Onderzoek
Historisch gezien had Lua geen ingebouwde stringinterpolatie, in tegenstelling tot sommige andere talen (bijv. Ruby, Python). Concatenatie met `..` was de gangbare methode. Lua 5.3 introduceerde `string.format` voor een schonere aanpak, vergelijkbaar met C's `printf`. **Alternatieven:** Naast het gebruik van de operator `..` of `string.format`, kun je ook een aangepaste interpolatiefunctie schrijven die `gsub` gebruikt voor patroonmatching. Maar waarom dingen ingewikkeld maken? Gebruik ingebouwde tools voor onderhoudbaarheid. **Implementatiedetails:** Weet dat frequente stringconcatenatie kan leiden tot prestatieproblemen. `string.format` is handig wanneer je controle nodig hebt over de opmaak, zoals het specificeren van nummerprecisie of opvulling.

## Zie Ook
- Lua Handleiding over Strings: http://www.lua.org/manual/5.4/manual.html#6.4
- 'Programmeren in Lua' over Strings: https://www.lua.org/pil/20.1.html
- Lua-gebruikers Wiki over Strings: http://lua-users.org/wiki/StringLibraryTutorial
