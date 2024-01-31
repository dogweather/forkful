---
title:                "Uthenting av delstrenger"
date:                  2024-01-20T17:45:57.320669-07:00
model:                 gpt-4-1106-preview
simple_title:         "Uthenting av delstrenger"

category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å trekke ut substringer betyr å hente ut bestemte deler av en streng. Programmerere gjør dette for å bearbeide, sjekke eller manipulere tekst på et mer detaljnivå.

## Hvordan:
Her er noen eksempler på hvordan man trekker ut substringer i Lua med funksjonen `string.sub`:

```lua
local tekst = "Hei, verden!"
local substr = string.sub(tekst, 5, 10)
print(substr)  -- Output: verden
```

En annen måte, med negative indekser for å telle baklengs:

```lua
local substr_baklengs = string.sub(tekst, -7, -2)
print(substr_baklengs)  -- Output: verden
```

## Dypdykk
Funksjonen `string.sub` har vært en del av Lua siden tidlige versjoner. Alternativer inkluderer mønstergjenkjenning med `string.match` for mer komplekse behov. I implementasjonsdetaljer holder Lua styr på strenger internt som sekvenser av byte, så substringsoperasjoner er effektive, men vær oppmerksom på at Lua ikke bruker UTF-8 som standard.

## Se Også
- Lua 5.4 referansemanual: https://www.lua.org/manual/5.4/
- Lua-string funksjoner: https://www.lua.org/manual/5.4/manual.html#6.4
- Lua-brukerwiki om mønstergjenkjenning: https://lua-users.org/wiki/StringLibraryTutorial
