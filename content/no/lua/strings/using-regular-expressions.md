---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:33.872899-07:00
description: "Regul\xE6re uttrykk i programmering tillater samsvarss\xF8king og manipulasjon\
  \ av strenger basert p\xE5 spesifikke m\xF8nstre. Programmerere bruker dem til oppgaver\u2026"
lastmod: 2024-02-19 22:05:00.183851
model: gpt-4-0125-preview
summary: "Regul\xE6re uttrykk i programmering tillater samsvarss\xF8king og manipulasjon\
  \ av strenger basert p\xE5 spesifikke m\xF8nstre. Programmerere bruker dem til oppgaver\u2026"
title: "Bruke regul\xE6re uttrykk"
---

{{< edit_this_page >}}

## Hva og hvorfor?

Regulære uttrykk i programmering tillater samsvarssøking og manipulasjon av strenger basert på spesifikke mønstre. Programmerere bruker dem til oppgaver som validering, søking og tekstmanipulasjon på grunn av deres allsidighet og effektivitet i håndtering av komplekse strengoperasjoner.

## Hvordan:

Lua støtter ikke regulære uttrykk på samme måte som språk som Perl eller Python. Isteden tilbyr det mønstersamsvarskapasiteter som dekker mange vanlige brukstilfeller av regulære uttrykk. For fullverdig støtte for regulære uttrykk, kan man bruke et tredjeparts bibliotek som `lrexlib`.

### Grunnleggende mønstersamsvar i Lua:

Lua tilbyr et kraftig system for mønstersamsvar som du kan bruke for enkle substitusjoner og søk:

```lua
-- Enkelt søk
local str = "Hallo, Verden!"
if string.find(str, "Verden") then
  print("Treff funnet!")
end
-- Utdata: Treff funnet!

-- Enkel substitusjon
local s = string.gsub("Lua er flott!", "flott", "fantastisk")
print(s)
-- Utdata: Lua er fantastisk!
```

### Fangst av delstrenger:

Du kan fange deler av strengen som samsvarer med mønstre:

```lua
local dato = "I dag er 17/05/2023."
local d, m, y = string.match(dato, "(%d+)/(%d+)/(%d+)")
print("Dag:", d, "Måned:", m, "År:", y)
-- Utdata: Dag: 17 Måned: 05 År: 2023
```

### Bruk av `lrexlib` for regulære uttrykk:

For å bruke faktiske regulære uttrykk, kan du installere og bruke `lrexlib`. Forutsatt at du har installert det (`luarocks install lrexlib-pcre`), kan du gjøre mer komplekse mønstersamsvar:

```lua
local rex = require 'rex_pcre'

local tekst = "Regnet i Spania blir hovedsakelig i sletten."
local regex = "\\bS\\w+"
local antall, feil = rex.gsub(tekst, regex, function(w)
  return w:upper()
end)
if feil then
  print("Feil:", feil)
else
  print("Endret tekst:", tekst)
  print("Substitusjoner gjort:", antall)
end
-- Eksempelutdata: Endret tekst: Regnet i SPANIA blir hovedsakelig i sletten.
-- Substitusjoner gjort: 3
```

De ovenstående eksemplene illustrerer grunnleggende bruk innenfor Lua sitt eget mønstersamsvarsystem og hvordan man kan utnytte kraften av regulære uttrykk via `lrexlib`. Enten du utfører enkle strengmanipulasjoner eller krever den fulle allsidigheten av regulære uttrykk, kan Lua, sammen med kraftige biblioteker, imøtekomme dine behov.
