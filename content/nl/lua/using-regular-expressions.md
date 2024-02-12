---
title:                "Reguliere expressies gebruiken"
aliases:
- nl/lua/using-regular-expressions.md
date:                  2024-01-28T22:09:49.994622-07:00
model:                 gpt-4-0125-preview
simple_title:         "Reguliere expressies gebruiken"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/lua/using-regular-expressions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Reguliere expressies, of regex, zijn patronen die worden gebruikt om combinaties van karakters in tekst te matchen. Programmeurs gebruiken ze voor zoeken, bewerken en manipuleren van strings omdat ze krachtig en efficiënt zijn.

## Hoe te:
Lua biedt basis ondersteuning voor patronen (zijn versie van regex) die je kunt gebruiken met string-matchfuncties. Hier is een snelle kennismaking:

```Lua
local text = "Hello Lua! 123"
-- Vind getallen in de tekst
local pattern = "%d+"
for match in string.gmatch(text, pattern) do
    print(match)
end
```
Uitvoer:
```
123
```

Om tekst te vervangen:

```Lua
local text = "Hello Lua! 123"
local pattern = "%d+"
local replacement = "456"
local new_text = string.gsub(text, pattern, replacement)

print(new_text)
```
Uitvoer:
```
Hello Lua! 456
```

## Diepgaand
Lua's patronen zijn niet zo rijk aan functies als regex in andere talen, maar ze zijn snel en dekken veel veelvoorkomende gebruikssituaties. Ze werden geïntroduceerd als een lichtgewicht oplossing voor string matching, om de complexiteit van traditionele regex-implementaties te vermijden.

Alternatieven zijn externe Lua-modules zoals `rex_pcre` of `lpeg`, die respectievelijk meer complete regex-implementaties of verschillende paradigma's voor patroonmatching bieden.

Lua's patroon-matchfuncties, zoals `string.find`, `string.match`, `string.gmatch` en `string.gsub`, werken met vooraf gedefinieerde patrooncodes zoals `%d` voor cijfers, `%s` voor spatiekarakters en `%a` voor letters, waardoor de implementatie eenvoudig is met minder overhead dan volledige regex-engines.

## Zie ook
- [Lua 5.4 Referentiehandleiding](https://www.lua.org/manual/5.4/manual.html#6.4.1)
