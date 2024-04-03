---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:48.257690-07:00
description: "Hvordan: I Lua kan skriving til stderr oppn\xE5s ved \xE5 bruke funksjonen\
  \ `io.stderr:write()`. Her er hvordan du kan skrive en enkel feilmelding til\u2026"
lastmod: '2024-03-13T22:44:40.946442-06:00'
model: gpt-4-0125-preview
summary: "I Lua kan skriving til stderr oppn\xE5s ved \xE5 bruke funksjonen `io.stderr:write()`."
title: Skriving til standardfeil
weight: 25
---

## Hvordan:
I Lua kan skriving til stderr oppnås ved å bruke funksjonen `io.stderr:write()`. Her er hvordan du kan skrive en enkel feilmelding til standardfeil:

```lua
io.stderr:write("Feil: Ugyldig inndata.\n")
```

Skulle du trenge å utvise en variabel eller kombinere flere datadelar, konkatenere dem innenfor skrivefunksjonen:

```lua
local feilmelding = "Ugyldig inndata."
io.stderr:write("Feil: " .. feilmelding .. "\n")
```

**Eksempel på utdata på stderr:**
```
Feil: Ugyldig inndata.
```

For mer komplekse scenarioer, eller når man arbeider med større applikasjoner, kan du vurdere tredjeparts loggføringsbiblioteker som LuaLogging. Med LuaLogging, kan du dirigere logger til forskjellige destinasjoner, inkludert stderr. Her er et kort eksempel:

Først, sørg for at LuaLogging er installert ved hjelp av LuaRocks:

```
luarocks install lualogging
```

Deretter, for å skrive en feilmelding til stderr ved hjelp av LuaLogging:

```lua
local logging = require("logging")
local logger = logging.stderr()
logger:error("Feil: Ugyldig inndata.")
```

Denne tilnærmingen tilbyr fordelen av standardisert loggføring tvers gjennom applikasjonen din, med den tilleggede fleksibiliteten av å sette loggnivåer (f.eks., ERROR, WARN, INFO) gjennom et enkelt API.
