---
title:                "Konvertering av en streng til små bokstaver"
html_title:           "Lua: Konvertering av en streng til små bokstaver"
simple_title:         "Konvertering av en streng til små bokstaver"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Konvertering av en streng til små bokstaver er en vanlig oppgave innen programmering. Dette refereres ofte som å "gjøre en streng til lowercase". Dette gjøres for å standardisere og forenkle databehandling, og for å unngå feil ved sammenligning av strenger.

# Hvordan:
```Lua
-- Eksempel på konvertering av streng til små bokstaver
local streng = "HEI ALLE!"

-- Med funksjonen string.lower()
print(string.lower(streng))

-- Uten funksjonen
print(streng:lower())
```
Output:
```lua
hei alle!
hei alle!
```

# Dypdykk:
Konvertering av strenger til små bokstaver har vært en del av programmering siden begynnelsen, da ASCII og Unicode ble introdusert for å representere bokstaver og symboler på datamaskiner. Før dette ble det brukt enkle numeriske verdier, som varierende mellom ulike systemer. Alternativene for å konvertere en streng til små bokstaver i Lua inkluderer bruk av regex og manuelt å iterere gjennom strengen og endre bokstavene individuelt. Sammenligning av strenger er viktig i programmering, og å ha en standardisert måte å konvertere dem til lavere case forenkler denne prosessen.

# Se også:
- [Lua.org](https://www.lua.org/)
- [Lua Tutorial](https://www.lua.org/manual/5.3/manual.html)
- [Lua String Library](https://www.lua.org/manual/5.3/manual.html#6.4)
- [ASCII vs. Unicode](https://www.utf8.com/)