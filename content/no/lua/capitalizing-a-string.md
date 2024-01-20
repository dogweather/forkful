---
title:                "Sette streng til store bokstaver"
html_title:           "Arduino: Sette streng til store bokstaver"
simple_title:         "Sette streng til store bokstaver"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å 'capitalise' en streng betyr å gjøre den første bokstaven i et ord eller hele strenger stor (store bokstaver). Programmerere bruker dette for å forbedre lesbarheten, spesielt når det gjelder brukergrensesnitt eller data som inneholder navn og titler.

## Hvordan:
```Lua
function capitalizeFirstLetter(str)
    return str:gsub("(%l)(%w*)", function(a, b) return string.upper(a) .. b end)
end

print(capitalizeFirstLetter("hallais folks, lua er gøy!"))
```

Output:
```
Hallais folks, lua er gøy!
```

## Dypdykk
Selv om 'capitalization' av strenger virker enkelt, er det ingen innebygd metode i Lua som håndterer dette direkte. Lua, som opprinnelig ble designet på 1990-tallet, har alltid vært minimalistisk. Derfor må vi definere våre egne funksjoner for oppgaver som å capitalise strenger. Alternativer til vår `capitalizeFirstLetter`-funksjon kan bruke mønstergjenkjenning eller arbeide med byte-verdier direkte for å manipulere tekst. Implementasjonsdetaljer kan variere avhengig av hvor strikt man ønsker å tolke 'capitalization', for eksempel om man ønsker å kun gjøre første bokstav i en setning stor, eller hver første bokstav i hvert ord.

## Se Også:
- [Lua's offisielle nettsted](https://www.lua.org)
- [String manipulation in Lua (PIL)](https://www.lua.org/pil/20.html)
- [Lua-users wiki: Patterns Tutorial](http://lua-users.org/wiki/PatternsTutorial)