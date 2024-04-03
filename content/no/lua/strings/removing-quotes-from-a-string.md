---
date: 2024-01-26 03:40:28.062487-07:00
description: "Hvordan: Slik kicker du de anf\xF8rselstegnene til kanten i Lua."
lastmod: '2024-03-13T22:44:40.918265-06:00'
model: gpt-4-0125-preview
summary: "Slik kicker du de anf\xF8rselstegnene til kanten i Lua."
title: "Fjerne anf\xF8rselstegn fra en streng"
weight: 9
---

## Hvordan:
Slik kicker du de anførselstegnene til kanten i Lua:

```lua
local function remove_quotes(str)
  return (str:gsub("^%p(.*)%p$", "%1"))
end

print(remove_quotes('"Hei, Verden!"'))     -- Hei, Verden!
print(remove_quotes("'Farvel, Anførselstegn!'"))  -- Farvel, Anførselstegn!
```

Bingo! De anførselstegnene forsvant som sokker i en tørketrommel.

## Dypdykk
Folk har skrubbet bort anførselstegn fra strenger siden språk kunne håndtere tekst, som praktisk talt er for alltid. I Lua, gjør `gsub`-funksjonen det tunge løftet, som håndterer mønstre som en skalpell for å fjerne anførselstegn. Alternativer? Sikkert, du kunne gå for regex i språk som støtter det, eller skrive din egen løkke som tygger gjennom hver karakter (gjesp, men hei, det er din tid).

Luas mønstersøking gir deg styrken av en regex-lite opplevelse uten å importere et helt bibliotek. Caret-tegnet (`^`) og dollartegnet (`$`) matcher starten og slutten av strengen, respektivt; `%p` matcher hvilket som helst tegnsettingstegn. Etter å ha ristet av seg den ledende og avsluttende tegnsettingen, fanger vi alt annet med `(.*),` og erstatter hele treffet med den fangstgruppen ved å bruke `" %1"`.

Husk at Luas mønstersøking ikke er så potent som fullverdige regex-motorer – for eksempel, den kan ikke telle eller backtracke. Denne enkelheten er både en velsignelse og en forbannelse, avhengig av hvilke anførselstegn du prøver å håndtere og hvor de gjemmer seg.

## Se Også
Dykk dypere inn i Luas mønstersøking med PiL (Programming in Lua)-boken: http://www.lua.org/pil/20.2.html

For ren eleganse, sjekk ut hvordan andre språk gjør det for sammenligning, start med Pythons `str.strip`: https://docs.python.org/3/library/stdtypes.html#str.strip
