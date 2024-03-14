---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:55.242693-07:00
description: "Getallen afronden betekent ze aanpassen naar het dichtstbijzijnde geheel\
  \ getal of gespecificeerde decimale plaats. Het is een basisprincipe in\u2026"
lastmod: '2024-03-13T22:44:50.932391-06:00'
model: gpt-4-0125-preview
summary: "Getallen afronden betekent ze aanpassen naar het dichtstbijzijnde geheel\
  \ getal of gespecificeerde decimale plaats. Het is een basisprincipe in\u2026"
title: Afronden van getallen
---

{{< edit_this_page >}}

## Wat & Waarom?
Getallen afronden betekent ze aanpassen naar het dichtstbijzijnde geheel getal of gespecificeerde decimale plaats. Het is een basisprincipe in programmeren om complexiteit te verminderen, prestaties te verhogen en voor momenten waarop precisie voorbij een bepaald punt geen waarde toevoegt.

## Hoe:
```lua
-- Basis afronding in Lua is niet ingebouwd, maar je kunt een functie definiëren:

function round(num)
    return num >= 0 and math.floor(num + 0.5) or math.ceil(num - 0.5)
end

print(round(3.5))  -- 4
print(round(2.3))  -- 2
print(round(-1.6)) -- -2

-- Om naar een specifieke decimale plaats af te ronden:
function round(num, decimalPlaces)
    local mult = 10^(decimalPlaces or 0)
    return math.floor(num * mult + 0.5) / mult
end

print(round(3.14159, 2)) -- 3.14
print(round(1.98765, 3))  -- 1.988
```

## Diepgaand
Lua bevat standaard geen afrondfunctie, in tegenstelling tot sommige andere talen. Geschiedenlijk moet je je eigen schrijven of een bibliotheek van derden gebruiken. Veelgebruikte oplossingen vertrouwen op `math.floor()` voor het afronden naar beneden en `math.ceil()` voor het afronden naar boven, gecombineerd met het toevoegen of aftrekken van 0.5 daarvoor, afhankelijk van het teken van het getal.

Alternatieven voor het maken van je eigen functie omvatten bibliotheken zoals "lua-users wiki" of "Penlight". Elk heeft zijn voordelen en nadelen, zoals extra functies of meer overhead.

Intern werken deze functies normaal gesproken door gebruik te maken van de manier waarop computers zwevende-kommagetallen opslaan. Het toevoegen van 0.5 aan een positieve float die je wilt afronden zal het over de drempel van de volgende geheel getalwaarde duwen, dus wanneer je `math.floor()` toepast, rondt het af naar beneden naar dat dichtstbijzijnde geheel getal.

## Zie Ook
- [Lua 5.4 Referentiehandleiding: De Wiskundige Functies](https://www.lua.org/manual/5.4/manual.html#6.7)
- [Penlight Lua Bibliotheken: Wiskunde](https://github.com/lunarmodules/Penlight)
