---
title:                "Få den gjeldende datoen"
html_title:           "Haskell: Få den gjeldende datoen"
simple_title:         "Få den gjeldende datoen"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å få dagens dato handler om å hente nåværende dato og tid. Programmerere gjør dette for å spore hendelser, generere tidsspesifikke data eller bare vise tiden.

## Slik gjør du:

Lua gir `os.date` funksjonen for å få dagens dato. Her er et enkelt eksempel på hvordan du bruker det.

```Lua
dato = os.date("*t") -- Få den nåværende datoen
print(os.date("I dag er det %d/%m/%Y", os.time(dato))) -- Skriver ut: I dag er det dd/mm/åååå
```

Dette vil hente og formatere datoen til en mer lesbar streng.

## Deep Dive

Historisk har Lua alltid hatt innebygde funksjoner for å håndtere dato og tid. `os.date` funksjonen har sin opprinnelse i C programming språket, som Lua er skrevet i. 

Alternativer for å få dagens dato i Lua inkluderer bruk av tredjeparts biblioteker som `luadate` eller `lua-timestamp`. Disse bibliotekene kan gi mer funksjonalitet, men de krever ekstra avhengigheter.

Implementasjonen av `os.date` funksjonen i Lua bruker C standardbibliotekets `time.h` headerfil. Dette gjør det enkelt og effektivt, men også plattformavhengig i noen tilfeller.

## Se Også

- Lua 5.4 manual: https://www.lua.org/manual/5.4/
- Lua users wiki: http://lua-users.org/wiki/
- Tredjeparts bibliotek - luadate: https://github.com/Tieske/date
- Tredjeparts bibliotek - lua-timestamp: https://github.com/leafo/lua-date