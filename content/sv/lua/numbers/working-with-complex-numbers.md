---
date: 2024-01-26 04:43:29.358433-07:00
description: "Komplexa tal utvidgar id\xE9n om en endimensionell tal-linje till ett\
  \ tv\xE5dimensionellt plan genom att inkludera en vinkelr\xE4t imagin\xE4r axel.\
  \ Programmerare\u2026"
lastmod: '2024-03-13T22:44:38.032176-06:00'
model: gpt-4-0125-preview
summary: "Komplexa tal utvidgar id\xE9n om en endimensionell tal-linje till ett tv\xE5\
  dimensionellt plan genom att inkludera en vinkelr\xE4t imagin\xE4r axel."
title: Att arbeta med komplexa tal
weight: 14
---

## Vad & Varför?
Komplexa tal utvidgar idén om en endimensionell tal-linje till ett tvådimensionellt plan genom att inkludera en vinkelrät imaginär axel. Programmerare arbetar med dem inom områden som signalbehandling, fluiddynamik och elektroteknik, där de är avgörande för att representera oscillationer och andra fenomen.

## Hur man gör:
I Lua kan du representera komplexa tal med tabeller. De grundläggande operationerna innefattar att addera, subtrahera, multiplicera och dela dessa tabeller. Så här gör du:

```lua
-- Definiera två komplexa tal som tabeller
local complex_a = { real = 3, imag = 5 }
local complex_b = { real = 2, imag = -4 }

-- Funktion för att addera två komplexa tal
local function add_complex(a, b)
  return { real = a.real + b.real, imag = a.imag + b.imag }
end

-- Exempel på utdata
print(add_complex(complex_a, complex_b))  -- { real = 5, imag = 1 }
```

## Fördjupning
Komplexa tal har funnits sedan 1500-talet, där de hjälpte till att lösa ekvationer som inte kunde knäckas med bara reella tal. Lua självt har inte en inbyggd komplex tal-typ. Dock är detta inga problem - du kan skapa dina egna manipulationer av komplexa tal med tabeller och funktioner, som visat ovan. Eller, om dina behov är mer omfattande, kan du ta en bibliotek som LuaComplex. Detta är ett fint val eftersom den är specifikt byggd för Lua och tar manuellt arbete från dina händer. Bibliotek som detta optimerar ofta operationer under huven, så de är snabbare än att rulla dina egna.

## Se också
För mer detaljerade exempel och avancerade operationer, kolla in dessa:

- LuaComplex bibliotek: https://github.com/davidm/lua-complex
- "Programming in Lua"-boken, för skapande av anpassade datatyper: https://www.lua.org/pil/11.1.html
- Wikipedia om komplexa tals användningar inom olika fält: https://en.wikipedia.org/wiki/Complex_number#Applications
