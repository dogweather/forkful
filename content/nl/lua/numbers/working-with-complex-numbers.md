---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:09.861026-07:00
description: "Complexe getallen breiden het idee van de eendimensionale getallenlijn\
  \ uit tot het tweedimensionale vlak door een loodrechte imaginaire as op te nemen.\u2026"
lastmod: '2024-03-13T22:44:50.931441-06:00'
model: gpt-4-0125-preview
summary: Complexe getallen breiden het idee van de eendimensionale getallenlijn uit
  tot het tweedimensionale vlak door een loodrechte imaginaire as op te nemen.
title: Werken met complexe getallen
weight: 14
---

## Wat & Waarom?
Complexe getallen breiden het idee van de eendimensionale getallenlijn uit tot het tweedimensionale vlak door een loodrechte imaginaire as op te nemen. Programmeurs werken ermee in velden zoals signaalverwerking, stromingsdynamica en elektrotechniek, waar ze essentieel zijn voor het vertegenwoordigen van oscillaties en andere verschijnselen.

## Hoe te:
In Lua kun je complexe getallen met tabellen voorstellen. De basisbewerkingen omvatten het optellen, aftrekken, vermenigvuldigen en delen van deze tabellen. Hier is hoe:

```lua
-- Definieer twee complexe getallen als tabellen
local complex_a = { real = 3, imag = 5 }
local complex_b = { real = 2, imag = -4 }

-- Functie om twee complexe getallen op te tellen
local function add_complex(a, b)
  return { real = a.real + b.real, imag = a.imag + b.imag }
end

-- Voorbeelduitvoer
print(add_complex(complex_a, complex_b))  -- { real = 5, imag = 1 }
```

## Diepgaande duik
Complexe getallen bestaan al sinds de 16e eeuw, helpend bij het oplossen van vergelijkingen die niet met enkel reële getallen gekraakt konden worden. Lua zelf heeft geen ingebouwd complex getaltype. Dit is echter geen groot probleem - je kunt je eigen complexe getallenmanipulaties vervaardigen met tabellen en functies, zoals hierboven getoond. Of, als je behoeften dieper gaan, pak een bibliotheek zoals LuaComplex. Dit is een prima keuze omdat het specifiek voor Lua is gebouwd en het handmatige werk van je overneemt. Bibliotheken zoals deze optimaliseren vaak ook operaties onder de motorkap, dus ze zijn sneller dan je eigen oplossingen bedenken.

## Zie Ook
Voor meer gedetailleerde voorbeelden en geavanceerde bewerkingen, bekijk deze:

- LuaComplex bibliotheek: https://github.com/davidm/lua-complex
- "Programming in Lua" boek, voor het creëren van aangepaste datatypes: https://www.lua.org/pil/11.1.html
- Wikipedia over complexe getallen hun gebruik in verschillende velden: https://nl.wikipedia.org/wiki/Complex_getal#Toepassingen
