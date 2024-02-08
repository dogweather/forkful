---
title:                "De lengte van een string vinden"
aliases:
- nl/lua/finding-the-length-of-a-string.md
date:                  2024-01-28T22:00:18.919233-07:00
model:                 gpt-4-0125-preview
simple_title:         "De lengte van een string vinden"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/lua/finding-the-length-of-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

De lengte van een tekst vinden betekent uitzoeken hoeveel tekens deze bevat. Programmeurs doen dit om invoer te valideren, tekst te manipuleren of simpelweg tekens te tellen voor verschillende taken.

## Hoe:

In Lua pak je de lengte van een string met de `#` operator. Eenvoudig en snel.

```lua
local mijnString = "Hallo, Lua!"
print(#mijnString)  -- Output: 11
```

Wat als je string nieuwe lijntekens bevat of leeg is?

```lua
local stringWithNewline = "Hallo\nLua!"
local emptyString = ""
print(#stringWithNewline)  -- Output: 10
print(#emptyString)         -- Output: 0
```

Zelfs met nieuwe lijntekens telt Lua elk teken. En ja, een lege string is 0 lang.

## Diepgaand

Vroeger waren strings in sommige talen lastiger. Je had misschien functies of methoden nodig om de lengte van een string te krijgen. Vandaag, in Lua, is het zo direct als het gebruiken van de `#` operator.

Alternatieven? Als je te maken hebt met Unicode-tekens, kan de `#` operator problemen veroorzaken met multibyte-tekens. In dat geval zou je bibliotheken zoals `utf8` verkennen. Lua 5.3 introduceerde deze ingebouwde bibliotheek.

```lua
local unicodeString = "こんにちは" -- Dat is "Hallo" in het Japans
print(#unicodeString)  -- Output kan verrassend zijn als je niet klaar bent voor multibyte-tekens!
print(utf8.len(unicodeString))  -- Output: 5 tekens zoals verwacht
```

Een detail dat de moeite waard is om op te merken: Lua houdt strings onveranderlijk en intern hergebruikt via een mechanisme genaamd string interning. Dit is netjes omdat het geheugen bespaart en stringlengte-operaties snel maakt.

## Zie Ook

- Lua 5.4 Referentiehandleiding: Stringmanipulatie – https://www.lua.org/manual/5.4/manual.html#6.4
- `utf8.len` functie – Duik in het correct omgaan met Unicode-teksten – https://www.lua.org/manual/5.4/manual.html#pdf-utf8.len
- Wat Lua-geschiedenis en informatie over string interning – https://www.lua.org/doc/hopl.pdf
