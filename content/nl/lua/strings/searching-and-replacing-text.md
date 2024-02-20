---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:14.807790-07:00
description: "Tekst zoeken en vervangen betekent specifieke tekenreeksen in een tekstblok\
  \ omwisselen met andere. Programmeurs doen dit voor taken zoals het herstellen\u2026"
lastmod: 2024-02-19 22:05:09.996322
model: gpt-4-0125-preview
summary: "Tekst zoeken en vervangen betekent specifieke tekenreeksen in een tekstblok\
  \ omwisselen met andere. Programmeurs doen dit voor taken zoals het herstellen\u2026"
title: Tekst zoeken en vervangen
---

{{< edit_this_page >}}

## Wat & Waarom?
Tekst zoeken en vervangen betekent specifieke tekenreeksen in een tekstblok omwisselen met andere. Programmeurs doen dit voor taken zoals het herstellen van fouten, het bijwerken van informatie of het formatteren van gegevens.

## Hoe:
De `string.gsub` functie van Lua is je eerste keuze voor zoeken en vervangen. Het werkt als volgt:

```lua
local text = "De snelle bruine vos springt over de luie hond."
local zoekTekst = "luie"
local vervangMet = "energieke"

local resultaat = string.gsub(text, zoekTekst, vervangMet)

print(resultaat)
```

Uitvoer:

```
De snelle bruine vos springt over de energieke hond.
```

Om ALLE voorkomens te vervangen, doet `gsub` dat standaard:

```lua
local text = "Appels zijn zoet. Appels zijn sappig."
local resultaat = string.gsub(text, "Appels", "Sinaasappels")

print(resultaat)
```

Uitvoer:

```
Sinaasappels zijn zoet. Sinaasappels zijn sappig.
```

## Diepere Duik
Tekst zoeken en vervangen is niet uniek voor Lua; het is een gemeenschappelijke functie in programmeertalen. Lua's `string.gsub` gaat terug naar zijn wortels van stringmanipulatie, en biedt een eenvoudige benadering om met patronen en vervangingen om te gaan.

Historisch gezien is `gsub` (globale substitutie) beïnvloed door Unix's `sed`-commando en Perl's krachtige patroonmatchingmogelijkheden. Lua's patronen, hoewel eenvoudiger dan de reguliere expressies die in andere talen worden gevonden, kunnen nog steeds complexe overeenkomsten aan met een beetje creativiteit.

Alternatieven voor `string.gsub` omvatten handmatig itereren door strings en het construeren van vervangingen - een meer foutgevoelige methode. Voor zware tekstverwerking kunnen speciale parsingbibliotheken worden gebruikt.

Wat implementatie betreft, kan `gsub` een functie als vervangingsargument nemen, waardoor programmatische controle over de substitutie mogelijk is.

```lua
local resultaat = string.gsub(text, "(%a+)", function(woord)
  return #woord > 4 and woord:upper() or woord
end)
```

Dit fragment zal woorden die langer zijn dan vier tekens in hoofdletters zetten.

## Zie Ook
- Het [Programming in Lua boek](https://www.lua.org/pil/), biedt diepgaande kennis van Lua's programmeerconcepten.
- Voor Lua's volledige mogelijkheden van stringpatronen, controleer de [Lua 5.4 Referentiehandleiding](https://www.lua.org/manual/5.4/manual.html#6.4.1).
