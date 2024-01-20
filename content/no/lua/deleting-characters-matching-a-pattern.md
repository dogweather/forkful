---
title:                "Slette tegn som samsvarer med et mønster"
html_title:           "Arduino: Slette tegn som samsvarer med et mønster"
simple_title:         "Slette tegn som samsvarer med et mønster"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å slette tegn som matcher et mønster, betyr å fjerne spesifikke tegn fra en streng basert på et forhåndsbestemt mønster. Dette er viktig for programmerere for å rengjøre, standardisere og manipulere data effektivt i programmer.

## Hvordan:

Her er et enkelt eksempel der vi sletter alle alfabetiske tegn fra en tekststring:

```Lua
s = "abc123"
s = s:gsub("%a", "")
print(s) -- Output: 123
```

Men vi kan også slette mer komplekse mønstre. Her er et eksempel der vi sletter alt mellom, inkludert klammene {}:

```Lua
s = "Hei {Norwegian} Venner"
s = s:gsub("{.-}", "")
print(s) -- Output: Hei  Venner
```

## Dypdykk 

Lua bruker mønstersammenligning inspirert av de regulære uttrykkene i POSIX og Perl, men Lua sitt mønstersystem er mer begrenset og har færre regler. 

Alternativene for å slette tegn som matcher et mønster inkluderer flere biblioteker og verktøy som Regular Expressions (Regex), men Lua’s innebygde `gsub` funksjon er enkel og lett å bruke for de fleste tilfeller.

Når det kommer til implementering, erstatter `gsub` funksjonen i Lua alle tilfeller av et mønster i en streng med en annen streng (eller fjerner det helt hvis den andre strengen er tom), og returnerer den reviderte strengen.

## Se Også:

-[Lua manual (5.4): Mønstersammenligning](https://www.lua.org/manual/5.4/manual.html#6.4.1)
-[Grunnleggende strengmanipulasjoner i Lua](https://learnxinyminutes.com/docs/lua/)
-[Lua Patterns](http://lua-users.org/wiki/PatternsTutorial)
-[Lua-Users: The power of patterns in Lua](http://lua-users.org/wiki/PatternsTutorial) 

Vennligst referer til disse kildene for ytterligere informasjon og bedre forståelse i bruk av mønstre i Lua.