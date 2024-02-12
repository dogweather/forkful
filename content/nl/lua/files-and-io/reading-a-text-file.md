---
title:                "Een tekstbestand lezen"
aliases:
- /nl/lua/reading-a-text-file/
date:                  2024-01-28T22:04:48.712798-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een tekstbestand lezen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/lua/reading-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een tekstbestand lezen betekent het laden van de inhoud in je programma. We doen dit om opgeslagen gegevens te verwerken, analyseren of weergeven, zoals instellingen, logboeken of gebruikersinvoer.

## Hoe:

Laten we bekijken hoe we een tekstbestand regel voor regel en vervolgens in één keer kunnen lezen.

```Lua
-- Het bestand regel voor regel lezen
local file = io.open("voorbeeld.txt", "r") -- Open het bestand om te lezen
if file then
  for line in file:lines() do -- Itereren over elke regel in het bestand
    print(line)
  end
  file:close() -- Sluit altijd het bestand als je klaar bent
else
  print("Kan bestand niet openen.")
end

-- Het hele bestand in één keer lezen
local file = io.open("voorbeeld.txt", "r") -- Open het bestand om te lezen
if file then
  local content = file:read("*a") -- Lees de volledige inhoud
  print(content)
  file:close() -- Sluit het bestand
else
  print("Kan bestand niet openen.")
end
```

Voorbeelduitvoer voor beide fragmenten, als `voorbeeld.txt` bevat:
```
Hallo, Lua!
```

De uitvoer zal zijn:
```
Hallo, Lua!
```

## Diepgaand

Historisch gezien is het lezen van bestanden een fundamentele operatie, daterend uit de begintijd van computers. In Lua wordt dit afgehandeld via een eenvoudig I/O-model met de `io` bibliotheek.

Hoewel `io.lines` en `io.read` gebruikelijke manieren zijn om toegang tot de inhoud van een bestand te krijgen, zijn er alternatieven zoals `lfs` (LuaFileSystem) voor geavanceerde bestandsbewerkingen.

Bij het lezen hanteert Lua buffering achter de schermen, maar voor grote bestanden moet je in delen lezen om hoog geheugengebruik te vermijden. 

Het gebruik van de `io` bibliotheek is eenvoudig, maar vergeet niet altijd bestanden te sluiten om lekken van middelen te voorkomen. Bij een fout retourneren Lua-bestandsbewerkingen `nil` en een foutmelding, die je moet afhandelen voor robuustheid.

## Zie Ook:

- [Lua 5.4 Referentiehandleiding: I/O](https://www.lua.org/manual/5.4/manual.html#6.8)
- [Leer Lua](https://learnxinyminutes.com/docs/lua/)
