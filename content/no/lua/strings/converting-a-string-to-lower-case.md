---
aliases:
- /no/lua/converting-a-string-to-lower-case/
date: 2024-01-20 17:38:57.115800-07:00
description: "\xC5 konvertere en streng til sm\xE5 bokstaver betyr \xE5 endre alle\
  \ store bokstaver i strengen til sine sm\xE5 bokstav-ekvivalenter. Programmerere\
  \ gj\xF8r dette for \xE5\u2026"
lastmod: 2024-02-18 23:08:54.003294
model: gpt-4-1106-preview
summary: "\xC5 konvertere en streng til sm\xE5 bokstaver betyr \xE5 endre alle store\
  \ bokstaver i strengen til sine sm\xE5 bokstav-ekvivalenter. Programmerere gj\xF8\
  r dette for \xE5\u2026"
title: "Konvertere en streng til sm\xE5 bokstaver"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å konvertere en streng til små bokstaver betyr å endre alle store bokstaver i strengen til sine små bokstav-ekvivalenter. Programmerere gjør dette for å standardisere tekstdata, for eksempel i søkefunksjoner eller når man sammenligner strenger uavhengig av brukerens input-casing.

## Hvordan gjøre det:
```Lua
-- Enkel strengkonvertering til små bokstaver
local tekst = "Hei, Skandinavia!"
local tekst_små = tekst:lower()

print(tekst_små)  -- Output: hei, skandinavia!
```

Her brukes `lower`-funksjonen til å gjøre hele strengen små bokstaver. Husk å lagre resultatet i en variabel om du trenger det videre i koden din.

```Lua
-- Sammenligne strenger, case-insensitive
local streng1 = "FJELL"
local streng2 = "fjell"

if streng1:lower() == streng2:lower() then
    print("Strengene er like.")
else
    print("Strengene er ikke like.")
end

-- Output: Strengene er like.
```

## Dypdykk
Før Lua hadde innebygde funksjoner for strengmanipulasjon, måtte utviklere gjøre dette manuelt, tegn for tegn, noe som var ineffektivt og feilutsatt. `string.lower` er nå standard og en del av Lua's standardbibliotek.

Alternativt kan man bruke mønstergjenkjenning og erstatning for tilsvarende konverteringer, men for rene småbokstav-tilfeller er `:lower()` mest direkte og effektivt.

Detaljene involverer vanligvis et enkelt tabell-oppslag der hver stor bokstav har en korresponderende liten bokstav. Lua (som i nyere versjoner) optimerer denne operasjonen for å være rask og lite ressurskrevende.

## Se også
Offisiell Lua-dokumentasjon av `string.lower`: https://www.lua.org/manual/5.4/manual.html#pdf-string.lower

Lua-gebruikersgids for mønstergjenkjenning og -erstatning: https://www.lua.org/pil/20.2.html

Forum for diskusjon om Lua-programmering: https://www.lua.org/forums.html
