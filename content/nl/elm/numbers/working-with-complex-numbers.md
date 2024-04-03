---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:57.320260-07:00
description: "Hoe te: Elm heeft geen ingebouwde ondersteuning voor complexe getallen,\
  \ dus je zult je eigen type en functies moeten cre\xEBren. Hier is een snelle opzet."
lastmod: '2024-03-13T22:44:50.718462-06:00'
model: gpt-4-0125-preview
summary: "Elm heeft geen ingebouwde ondersteuning voor complexe getallen, dus je zult\
  \ je eigen type en functies moeten cre\xEBren."
title: Werken met complexe getallen
weight: 14
---

## Hoe te:
Elm heeft geen ingebouwde ondersteuning voor complexe getallen, dus je zult je eigen type en functies moeten creëren. Hier is een snelle opzet:

```Elm
type alias Complex =
    { real : Float, imaginary : Float }

add : Complex -> Complex -> Complex
add a b =
    { real = a.real + b.real, imaginary = a.imaginary + b.imaginary }

-- Voorbeeldgebruik:
a = { real = 3, imaginary = 2 }
b = { real = 1, imaginary = -4 }

som = add a b
-- som is { real = 4.0, imaginary = -2.0 }
```

## Diepere Duik
Historisch gezien werden complexe getallen niet altijd geaccepteerd. Ze werden een game-changer in de 16e eeuw om kubieke vergelijkingen op te lossen. Alternatieven in andere talen zoals Python bieden ingebouwde ondersteuning voor complexe getallen met operaties direct uit de doos. Elm vereist een doe-het-zelfbenadering, zoals je hebt gezien. Maar je kunt het zo geavanceerd maken als nodig is, door vermenigvuldiging, deling en andere bewerkingen te bouwen, prestatieproblemen afstemmen.

## Zie Ook
- Elm's Officiële Documentatie: https://package.elm-lang.org/ voor het creëren van aangepaste typen en het beheersen van Elm-basics.
- Liefhebbers van wiskundegeschiedenis zouden "An Imaginary Tale" door Paul J. Nahin kunnen bekijken voor een reis door de tijd van complexe getallen.
- Duik in wiskunde-georiënteerde programmeeruitdagingen op Project Euler (https://projecteuler.net) om je tovenarij met complexe getallen toe te passen.
