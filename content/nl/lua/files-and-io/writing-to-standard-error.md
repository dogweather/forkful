---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:32.557068-07:00
description: "Schrijven naar standaardfout (stderr) laat je programma praten over\
  \ fouten zonder de standaarduitvoer (stdout) te verstoppen. Het is een duidelijk\
  \ signaal\u2026"
lastmod: '2024-03-13T22:44:50.953664-06:00'
model: gpt-4-0125-preview
summary: "Schrijven naar standaardfout (stderr) laat je programma praten over fouten\
  \ zonder de standaarduitvoer (stdout) te verstoppen. Het is een duidelijk signaal\u2026"
title: Schrijven naar standaardfout
---

{{< edit_this_page >}}

## Wat & Waarom?

Schrijven naar standaardfout (stderr) laat je programma praten over fouten zonder de standaarduitvoer (stdout) te verstoppen. Het is een duidelijk signaal naar gebruikers en andere programma's dat er aandacht nodig is.

## Hoe te:

Lua communiceert met stderr via `io.stderr`. Zo print je een eenvoudige foutmelding:

```lua
io.stderr:write("Fout: Er is iets misgegaan!\n")
```

Voorbeelduitvoer op stderr:
```
Fout: Er is iets misgegaan!
```

Je kunt het fancy maken en combineren met foutafhandeling:

```lua
if not file then
    io.stderr:write("Fout: Bestand niet gevonden.\n")
    os.exit(1) -- stop met een niet-nul exitcode
end
```

## Diepgaand Onderzoek

Lang geleden kregen computers twee aparte stromen voor uitvoer—stdout voor de hoofdgegevens, stderr voor de oepsjes. Lua hield deze Unix-conventie in stand. Soms leiden mensen stdout om (zoals naar een bestand) maar willen ze fouten toch op het scherm hebben. Daar komt stderr om de hoek kijken.

Alternatieven? Sommigen schrijven naar een logbestand, gebruiken een logboekbibliotheek, of sturen het over netwerken. Maar stderr is laagdrempelig voor eenvoudige zaken.

Wat implementatie betreft, Lua’s `io.stderr` is een bestandshandle. Het is net als `io.stdout` of `io.stdin`, klaar voor gebruik zonder gedoe. Achter de schermen, of het nu een tekstbestand of een terminal is, Lua zweet niet—`io.stderr` handelt het af.

## Zie Ook

Duik dieper of krijg wat context:

- De Lua 5.4 Referentiehandleiding: http://www.lua.org/manual/5.4/
- Unix-filosofie: https://nl.wikipedia.org/wiki/Unix-filosofie
- Leer meer over `os.exit`: http://www.lua.org/pil/21.3.html
- Een rondleiding door Lua's invoer- en uitvoermogelijkheden: http://www.lua.org/pil/21.html
