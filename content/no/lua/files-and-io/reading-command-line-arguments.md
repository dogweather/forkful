---
date: 2024-01-20 17:56:31.589614-07:00
description: Lesing av kommandolinjeargumenter lar oss ta imot data fra brukere utenfor
  et Lua-script. Slik kan programmer bli mer fleksible og brukertilpassede.
lastmod: '2024-02-25T18:49:39.117969-07:00'
model: gpt-4-1106-preview
summary: Lesing av kommandolinjeargumenter lar oss ta imot data fra brukere utenfor
  et Lua-script. Slik kan programmer bli mer fleksible og brukertilpassede.
title: Lese kommandolinjeargumenter
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Lesing av kommandolinjeargumenter lar oss ta imot data fra brukere utenfor et Lua-script. Slik kan programmer bli mer fleksible og brukertilpassede.

## Slik gjør du:
```lua
-- Lagret som example.lua
for i = 1, #arg do
  print(i, arg[i])
end
```
Kjør scriptet: `lua example.lua Hei Verden!`
Forventet utskrift:
```
1	Hei
2	Verden!
```

## Dypdykk
Lua håndterer kommandolinjeargumenter via `arg`-tabellen. Denne funksjonaliteten, arvet fra C, er rett fram og enkel. Alternativer som `getopt` fra andre språk har ikke et direkte svar i Lua, men kan etterlignes med funksjoner. For å lese flagg og mer komplekse scenarier, bruker utviklere ofte eksterne bibliotek som `lapp` eller skriver egne parsers. `arg` er dog perfekt for enkle brukstilfeller.

## Se Også
- Lua-users wiki om argumenter: http://lua-users.org/wiki/CommandLineArguments
- "Programming in Lua" for avanserte parsers: https://www.lua.org/pil/21.2.html
- Lapp bibliotek: http://stevedonovan.github.io/Penlight/api/libraries/pl.lapp.html
