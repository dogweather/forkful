---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:11.836815-07:00
description: "\xC5 hente den aktuelle datoen i programmering er en avgj\xF8rende oppgave\
  \ for en rekke applikasjoner, inkludert logging, tidsstempelhendelser, eller\u2026"
lastmod: '2024-03-13T22:44:40.940687-06:00'
model: gpt-4-0125-preview
summary: "\xC5 hente den aktuelle datoen i programmering er en avgj\xF8rende oppgave\
  \ for en rekke applikasjoner, inkludert logging, tidsstempelhendelser, eller\u2026"
title: "F\xE5 dagens dato"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å hente den aktuelle datoen i programmering er en avgjørende oppgave for en rekke applikasjoner, inkludert logging, tidsstempelhendelser, eller planlegging av oppgaver. I Lua, gir denne funksjonaliteten programmerere muligheten til å håndtere dato- og tidsoperasjoner sømløst innenfor deres applikasjoner, og sikrer at deres programvare kan samhandle med sanntidsdata effektivt.

## Hvordan:

Lua tilbyr `os.date`-funksjonen for å få den aktuelle datoen og tiden. Funksjonen kan brukes uten argumenter for å få en formatert streng, eller med format-spesifikatorer for å tilpasse utdata. Slik bruker du den:

```lua
-- Henter den nåværende datoen og tiden som en formatert streng
print(os.date())  -- f.eks, Thu Mar  3 14:02:03 2022

-- Tilpasser utdataformatet
-- %Y for år, %m for måned, %d for dag, %H for time, %M for minutter
print(os.date("%Y-%m-%d %H:%M"))  -- f.eks, 2022-03-03 14:02
```

For mer sofistikert manipulering av dato og tid, har Lua ikke innebygde biblioteker så rike som noen andre programmeringsspråk. Du kan imidlertid bruke tredjeparts biblioteker som `lua-date` (https://github.com/Tieske/date). Dette biblioteket tilbyr mer omfattende funksjonaliteter for manipulering av datoer og tider. Slik kan du bruke den:

Først, sørg for at du har installert `lua-date`-biblioteket. Du kan vanligvis installere det ved å bruke LuaRocks med følgende kommando:

```bash
luarocks install lua-date
```

Deretter kan du bruke det i Lua-skriptet ditt slik:

```lua
local date = require("date")

-- Oppretter et datobjekt for den nåværende datoen og tiden
local now = date()

print(now:fmt("%Y-%m-%d %H:%M:%S"))  -- f.eks, 2022-03-03 14:02:03
```

Dette eksemplet demonstrerer opprettelsen av et `date`-objekt som representerer det nåværende øyeblikket, som du deretter kan formatere på en lignende måte som `os.date`-funksjonen, men med tilført fleksibilitet og alternativer gitt av `lua-date`-biblioteket.
