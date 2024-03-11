---
date: 2024-01-26 04:43:09.880899-07:00
description: "Komplekse tall utvider ideen om den \xE9ndimensjonale tallinjen til\
  \ det todimensjonale planet ved \xE5 inkludere en vinkelrett imagin\xE6r akse. Programmerere\u2026"
lastmod: '2024-03-11T00:14:14.493644-06:00'
model: gpt-4-0125-preview
summary: "Komplekse tall utvider ideen om den \xE9ndimensjonale tallinjen til det\
  \ todimensjonale planet ved \xE5 inkludere en vinkelrett imagin\xE6r akse. Programmerere\u2026"
title: "\xC5 jobbe med komplekse tall"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Komplekse tall utvider ideen om den éndimensjonale tallinjen til det todimensjonale planet ved å inkludere en vinkelrett imaginær akse. Programmerere arbeider med dem i felt som signalbehandling, fluiddynamikk og elektroteknikk, der de er essensielle for å representere svingninger og andre fenomener.

## Hvordan:
I Lua kan du representere komplekse tall med tabeller. De grunnleggende operasjonene involverer å legge til, trekke fra, multiplisere og dele disse tabellene. Slik gjør du det:

```lua
-- Definer to komplekse tall som tabeller
local complex_a = { real = 3, imag = 5 }
local complex_b = { real = 2, imag = -4 }

-- Funksjon for å legge til to komplekse tall
local function add_complex(a, b)
  return { real = a.real + b.real, imag = a.imag + b.imag }
end

-- Eksempel på utdata
print(add_complex(complex_a, complex_b))  -- { real = 5, imag = 1 }
```

## Dypdykk
Komplekse tall har vært rundt siden det 16. århundre, og hjelper til med å løse ligninger som ikke kunne knekkes kun med reelle tall. Lua selv har ikke en innebygd komplekst talltype. Imidlertid er dette ingen stor sak - du kan lage dine egne komplekse tallmanipulasjoner ved hjelp av tabeller og funksjoner, som vist ovenfor. Eller, hvis dine behov er dypere, kan du få tak i et bibliotek som LuaComplex. Dette er et godt valg fordi det er spesielt laget for Lua og tar det manuelle arbeidet av dine skuldre. Biblioteker som dette optimaliserer ofte operasjoner under panseret, så de er raskere enn å rulle dine egne.

## Se Også
For mer detaljerte eksempler og avanserte operasjoner, sjekk disse ut:

- LuaComplex-bibliotek: https://github.com/davidm/lua-complex
- "Programming in Lua"-boken, for opprettelse av egendefinerte datatyper: https://www.lua.org/pil/11.1.html
- Wikipedia om komplekse tallers bruk i ulike felt: https://en.wikipedia.org/wiki/Complex_number#Applications
