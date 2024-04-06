---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:36.712082-07:00
description: "Hoe te: Lua, ontstaan in 1993, hield het simpel en recht door zee. Het\
  \ is lichtgewicht, makkelijk in te bedden, en zijn tabeldatastructuren zijn een\u2026"
lastmod: '2024-04-05T21:53:50.960710-06:00'
model: gpt-4-0125-preview
summary: Lua, ontstaan in 1993, hield het simpel en recht door zee.
title: Een nieuw project starten
weight: 1
---

## Hoe te:
```Lua
-- Laten we beginnen met een nieuw Lua project

-- 1. Hallo Wereld - De klassieke start
print("Hallo, Wereld!")

-- Voorbeelduitvoer: Hallo, Wereld!

-- 2. Een functie definiëren - Een stap verder
function groet(naam)
    print("Hallo, " .. naam .. "!")
end

-- Roep de functie aan met een naam
groet("Lua Programmeur")

-- Voorbeelduitvoer: Hallo, Lua Programmeur!

-- 3. Gebruikmaken van tabellen - Lua's manier om datastructuren te hanteren
local inventaris = {
    ["appels"] = 10,
    ["sinaasappels"] = 5,
    ["bananen"] = 3
}

-- Voeg een functie toe om de inventaris bij te werken
function voegFruitToe(fruit, hoeveelheid)
    if inventaris[fruit] then
        inventaris[fruit] = inventaris[fruit] + hoeveelheid
    else
        inventaris[fruit] = hoeveelheid
    end
end

-- Roep de functie aan om de inventaris bij te werken
voegFruitToe("appels", 5)

-- Geef de bijgewerkte inventaristelling voor appels weer
print("Appels in inventaris: " .. inventaris["appels"])

-- Voorbeelduitvoer: Appels in inventaris: 15
```

## Diepgaande duik
Lua, ontstaan in 1993, hield het simpel en recht door zee. Het is lichtgewicht, makkelijk in te bedden, en zijn tabeldatastructuren zijn een flexibele manier om de gegevens van je project te organiseren. In tegenstelling tot andere talen die een duizelingwekkende reeks datatypes kunnen bieden, houdt Lua het bij een paar kernsoorten en gebruikt het tabellen intelligent om het te compenseren. Wat betreft alternatieven, je hebt er genoeg – Python, Ruby, Node.js, en meer, elk met hun eigen setup eigenaardigheden en bibliotheken. Maar als je een nette, wendbare taal zoekt voor een snelle start of inbedding, dan is Lua jouw must-go. Wat implementatie betreft, draait Lua helemaal om functies, tabellen, en eenvoud. Het gebrek aan redundantie (denk aan klassen of complexe overervingen) is geen gebrek aan kracht; het is een ontwerpkeuze om je soepel op je coderingsreis te houden.

## Zie ook
- [Officiële Lua Documentatie](https://www.lua.org/manual/5.4/)
- [Programmeren in Lua (Eerste editie)](https://www.lua.org/pil/contents.html)
- [Leer Lua in Y minuten](https://learnxinyminutes.com/docs/lua/)
