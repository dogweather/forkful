---
title:                "Gjøre en streng stor"
html_title:           "Lua: Gjøre en streng stor"
simple_title:         "Gjøre en streng stor"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

Title: Håndtering av store bokstaver i strenger i Lua

## Hva Og Hvorfor?

En stor bokstavstreng er en streng der det første tegnet i hvert ord er i store bokstaver. Vi bruker dette i programmering for å gjøre teksten mer lesbar, eller for å oppfylle bestemte formatkrav.

## Hvordan du:

For å endre en streng til store bokstaver i Lua, vil vi bruke innebygde funksjoner:

```Lua
str = "hei verden fra lua"
cap_str = str:gsub("(%a)([%w_]*)", function(first, rest) return first:upper()..rest:lower() end)

print(cap_str)
```

Dette vil skrive ut:

```Lua
Hei Verden Fra Lua
```

## Dypdykk

Strengkapitalisering, som mange andre funksjoner, ble en del av Lua-biblioteket på et tidlig tidspunkt på grunn av dets brukbarhet og popularitet i behandling av tekstdokumenter.

Lua tilbyr også funksjonene `string.upper()` og `string.lower()` som kan gjøre alle tegnene i strengen til henholdsvis store bokstaver og små bokstaver. Men for å endre kun det første tegnet av hvert ord til stort, bruker vi funksjonen `string.gsub()` til å finne og erstatte undersekvenser i strengen.

Det er viktig å merke seg at denne metoden ikke vil fungere riktig for strenger som inneholder ikke-bokstavtegn på steder hvor vanlige bokstaver ville vært.

## Se også

- Lua 5.3 referanse manual String Library: https://www.lua.org/manual/5.3/manual.html#6.4
- Programets ydeevne med Lua string funksjoner: https://www.lua.org/pil/11.4.html
- Litt mer om Lua string mønster matching: https://www.lua.org/pil/20.2.html