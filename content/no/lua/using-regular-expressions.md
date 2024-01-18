---
title:                "Å bruke regulære uttrykk"
html_title:           "Lua: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Regular expressions (regulære uttrykk) er et uttrykk i programmering som brukes til å søke og manipulere tekststrenger. Dette er spesielt nyttig når man skal finne eller endre deler av tekst som følger et gitt mønster. Programmere bruker regelmessig uttrykk for å effektivt behandle store mengder tekst og automatisere repetitiv oppgaver.

## Slik gjør du det:
Kodingseksemplene nedenfor bruker Lua sin standardmodul `string` for å demonstrere hvordan man kan bruke regulære uttrykk.

```Lua 
-- Søke etter et mønster i en tekststreng
local text = "Hei, mitt navn er Lua"
local pattern = "Lua"
print(string.match(text, pattern)) 
-- Output: Lua

-- Erstatt deler av tekst basert på et mønster
local email = "navn@eksempel.com"
local pattern = "@"
local replacement = "[at]"
print(string.gsub(email, pattern, replacement)) 
-- Output: navn[at]eksempel.com

-- Finne deler av tekst som matcher et mønster og legge dem i en tabell
local sentence = "Lua er et morsomt programmeringsspråk"
local pattern = "%a+"
local words = {}
for word in string.gmatch(sentence, pattern) do
  table.insert(words, word)
end
print(table.concat(words, ", ")) 
-- Output: Lua, er, et, morsomt, programmeringsspråk
```

## Dypdykk:
Regulære uttrykk ble introdusert på 1950-tallet og har siden blitt en viktig del av mange programmeringsspråk, inkludert Lua. Alternativer til å bruke regulære uttrykk inkluderer å bruke innebygde funksjoner som `string.find` og `string.sub` eller å lage egne funksjoner for å håndtere tekstbehandling. Implementasjonen av regulære uttrykk varierer mellom ulike programmeringsspråk, men de følger stort sett samme mønster og uttrykk.

## Se også:
For mer informasjon om regulære uttrykk og deres bruk i Lua, kan du sjekke ut følgende ressurser:

- Offisiell Lua dokumentasjon for `string`-modulen: https://www.lua.org/manual/5.3/manual.html#6.4
- Online regex tester og dokumentasjon: https://regex101.com/
- Tutorial om regulære uttrykk i Lua: https://www.lua.org/pil/20.2.html