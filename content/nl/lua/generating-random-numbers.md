---
title:                "Willekeurige getallen genereren"
date:                  2024-01-28T22:01:01.330893-07:00
model:                 gpt-4-0125-preview
simple_title:         "Willekeurige getallen genereren"

category:             "Lua"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/lua/generating-random-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het genereren van willekeurige getallen in programmeren gaat over het produceren van onvoorspelbare numerieke waarden die gebruikt kunnen worden voor een verscheidenheid aan doeleinden zoals simulaties, spellen of beveiligingstoepassingen. Programmeurs maken gebruik van deze functie om een element van onzekerheid te introduceren of de variabiliteit van het echte leven na te bootsen in hun projecten.

## Hoe:

Lua biedt ingebouwde ondersteuning voor het genereren van willekeurige getallen via de `math.random` functie. Deze functie kan op meerdere manieren gebruikt worden, afhankelijk van de gewenste uitvoer:

1. **Een willekeurig drijvendekommagetal genereren tussen 0 en 1:**

```Lua
print(math.random())
```

Een voorbeelduitvoer kan zijn `0.13117647051304`. Elke uitvoering produceert een andere waarde.

2. **Een willekeurige integer genereren binnen een opgegeven bereik:**

Om een willekeurige integer te produceren tussen twee grenzen, inclusief, moet je eerst de seed instellen met `math.randomseed(os.time())` voor variabiliteit, vervolgens `math.random` aanroepen met twee argumenten:

```Lua
math.randomseed(os.time())
print(math.random(1, 10)) -- Genereert een willekeurige integer tussen 1 en 10
```

Een voorbeelduitvoer kan zijn `7`. Weer zal de uitvoer variëren bij elke uitvoering.

Het is cruciaal om de seed in te stellen met `math.randomseed`, omdat zonder dit, `math.random` dezelfde reeks getallen zou kunnen genereren elke keer dat een programma wordt uitgevoerd. Doorgaans zorgt seeden met de huidige tijd, `os.time()`, voor verschillende reeksen per uitvoering.

## Diepere Duik

Het mechanisme dat ten grondslag ligt aan de generatie van willekeurige getallen in Lua (en de meeste programmeertalen) is niet echt willekeurig maar pseudowillekeurig, gegenereerd door een algoritme. Deze pseudowillekeurige getallengeneratoren (PRNG's) zijn deterministisch en vereisen een seedwaarde om de reeks van getallengeneratie te beginnen. De keuze van seeden is cruciaal voor de kwaliteit van de willekeurigheid, wat de reden is waarom het gebruik van de huidige tijd een gangbare praktijk is.

Historisch gezien zijn de capaciteiten voor het genereren van willekeurige getallen in Lua geëvolueerd. Eerdere versies waren afhankelijk van de C-standaardbibliotheek `rand()` functie, die varieerde in kwaliteit en prestatie over implementaties. De huidige versie van Lua verbetert dit mogelijk door robuustere mechanismen te gebruiken, afhankelijk van het onderliggende platform, wat een grotere consistentie en nut biedt bij het genereren van willekeurige getallen.

Voor projecten die cryptografische niveau willekeurigheid vereisen, is de ingebouwde Lua-functionaliteit mogelijk niet voldoende vanwege de deterministische aard van PRNG's. In dergelijke gevallen wenden programmeurs zich vaak tot externe bibliotheken of systeemspecifieke API's die niet-deterministische willekeurige getallen kunnen bieden die geschikt zijn voor toepassingen met hoge beveiliging.
