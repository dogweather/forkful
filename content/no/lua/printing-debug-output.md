---
title:                "Utskrift av feilsøkingsutdata"
html_title:           "Lua: Utskrift av feilsøkingsutdata"
simple_title:         "Utskrift av feilsøkingsutdata"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/printing-debug-output.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Printing debug output er en måte for utviklere å få informasjon om hva som skjer i en kode mens den kjører. Det kan hjelpe til med å finne og løse feil, samt forstå hvordan koden oppfører seg.

# Slik gjør du det:
```Lua
-- Definisjon av funksjon
function sum(a, b)
  print("Legger sammen tallene " .. a .. " og " .. b)
  return a + b
end

-- Kaller funksjon og printer resultatet
print("Resultatet er " .. sum(5, 7))
```
```Resultatet er 12```

# Dypdykk:
Printing debug output har vært en vanlig praksis blant utviklere i lang tid, og er fortsatt en viktig del av feilsøking og forståelse av kode. Det finnes også alternativer til å bruke print funksjonen, som for eksempel å bruke en debugger.

Det finnes flere måter å implementere printing debug output i Lua på. En vanlig metode er å bruke funksjonen ```print()```, men det finnes også biblioteker som tilbyr mer avanserte måter å logge informasjon på.

# Se også:
- [Lua.org](https://www.lua.org/) - Offisiell nettside for Lua-programmeringsspråket.
- [Lua Users Wiki](https://lua-users.org/) - Samfunnsdrevet wiki med nyttig informasjon om Lua.
- [LuaTut](http://guitsaru.com/lua/lua00.php) - Nybegynner-vennlig Lua tutorial.
- [Debugging in Lua](http://lua-users.org/wiki/DebuggingLuaCode) - Informasjon om feilsøking og debugging i Lua.