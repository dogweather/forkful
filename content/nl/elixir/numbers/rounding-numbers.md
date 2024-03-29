---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:51.460870-07:00
description: "Getallen afronden betekent dat je ze aanpast naar een nabijgelegen waarde\
  \ voor eenvoud of om te voldoen aan een bepaalde precisie. Het is nuttig voor het\u2026"
lastmod: '2024-03-13T22:44:50.456974-06:00'
model: gpt-4-0125-preview
summary: "Getallen afronden betekent dat je ze aanpast naar een nabijgelegen waarde\
  \ voor eenvoud of om te voldoen aan een bepaalde precisie. Het is nuttig voor het\u2026"
title: Afronden van getallen
---

{{< edit_this_page >}}

## Wat & Waarom?
Getallen afronden betekent dat je ze aanpast naar een nabijgelegen waarde voor eenvoud of om te voldoen aan een bepaalde precisie. Het is nuttig voor het verbeteren van de leesbaarheid, het verminderen van opslagruimte, of om te voldoen aan domeinspecifieke behoeften, zoals geldberekeningen waarbij je wilt afronden op de dichtstbijzijnde cent.

## Hoe:
In Elixir, kun je `Float.round/2` gebruiken om een kommagetal af te ronden. Je kunt het aantal decimale cijfers specificeren dat je wilt behouden. Zo werkt het:

```elixir
# Een getal afronden op geen decimale plaatsen
Float.round(3.14159) # => 3.0

# Een getal afronden op 2 decimale plaatsen
Float.round(3.14159, 2) # => 3.14

# Een getal afronden met een negatieve precisie naar het dichtstbijzijnde 10
Float.round(123.456, -1) # => 120.0
```

## Diepere Duik
Het afronden van getallen is een klassiek probleem in de informatica—zozeer zelfs dat de keuze van afrondingsstrategie financiële systemen, wetenschappelijke berekeningen en meer kan beïnvloeden. Elixir's `Float.round/2` gebruikt standaard "half omhoog" afronden, wat lijkt op de traditionele afronding die in de wiskundeles wordt onderwezen.

Als je andere soorten afronding nodig hebt, laat Elixir je je eigen maken. Overweeg bijvoorbeeld "floor" afronden (altijd naar beneden) of "ceiling" afronden (altijd naar boven). Je zou respectievelijk `Float.floor/1` of `Float.ceil/1` gebruiken.

```elixir
# Floor afronden
Float.floor(3.999) # => 3.0

# Ceiling afronden
Float.ceil(3.001) # => 4.0
```

Deze alternatieven helpen het afronden aan te passen aan de exacte behoeften van je toepassing, of het nu gaat om financiële berekeningen, grafische rendering of data-approximatie.

## Zie Ook
Voor meer informatie over Elixir's afrondingsfuncties en kommagetallen:

- Elixir's officiële documentatie over `Float`: https://hexdocs.pm/elixir/Float.html
- IEEE-standaard voor vlottende-komma rekenkunde (IEEE 754): https://ieeexplore.ieee.org/document/4610935
