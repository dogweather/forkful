---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:28.666190-07:00
description: "Het verkrijgen van de huidige datum in de programmering stelt ons in\
  \ staat om bij te houden wanneer dingen gebeuren. We hebben tijdstempels nodig voor\u2026"
lastmod: '2024-03-13T22:44:50.947834-06:00'
model: gpt-4-0125-preview
summary: "Het verkrijgen van de huidige datum in de programmering stelt ons in staat\
  \ om bij te houden wanneer dingen gebeuren. We hebben tijdstempels nodig voor\u2026"
title: Het huidige datum ophalen
weight: 29
---

## Wat & Waarom?

Het verkrijgen van de huidige datum in de programmering stelt ons in staat om bij te houden wanneer dingen gebeuren. We hebben tijdstempels nodig voor logboeken, records, of gewoon om een "Gelukkig Nieuwjaar" bericht op het juiste moment te gooien.

## Hoe te:

In Lua, het grijpen van de huidige datum en tijd is een fluitje van een cent met de `os.date` functie. Bekijk het:

```lua
local huidige_tijd = os.date("*t")  -- krijgt tabel met datum en tijd componenten
print("Jaar:", huidige_tijd.year)
print("Maand:", huidige_tijd.month)
print("Dag:", huidige_tijd.day)

-- Wil je in plaats daarvan een geformatteerde string? Gemakkelijk.
print(os.date("%Y-%m-%d")) -- print in YYYY-MM-DD formaat
```

Voorbeelduitvoer:
```
Jaar: 2023
Maand: 4
Dag: 14
2023-04-14
```

## Diepere Duik

Lua's `os.date` bestaat al sinds de vroegste dagen, een basisvoorziening wanneer je de datum/tijd nodig hebt. Het is gebaseerd op de C `time.h` bibliotheekfuncties, dus het heruitvindt het wiel niet – Lua houdt het vertrouwd.

Alternatieven? Natuurlijk, je kunt ook `os.time` gebruiken om de seconden sinds het UNIX-tijdperk te krijgen en ermee spelen, of externe bibliotheken gebruiken voor bredere functionaliteit indien nodig. Maar `os.date` en `os.time` dekken de meeste bases prima.

Implementatie wijs, `os.date("*t")` geeft je een tabel met jaar, maand, dag, en meer. Formatteer het met `os.date()` door een formatteerstring door te geven, zoals `"%Y-%m-%d"` voor een standaard datum.

Pro-tip: Werken met tijdzones? `os.date` kan dat ook aan – gebruik de `!"` prefix in je formatteerstring, en Lua zal de Gecoördineerde Universele Tijd (UTC) gebruiken in plaats van de lokale tijd.

## Zie Ook

- Lua's `os` bibliotheek documentatie: http://www.lua.org/manual/5.4/manual.html#6.9
- Online Lua demo-omgeving om codefragmenten te testen: https://www.lua.org/cgi-bin/demo
- Formaatspecificaties voor `os.date`: https://www.lua.org/manual/5.4/manual.html#pdf-os.date
