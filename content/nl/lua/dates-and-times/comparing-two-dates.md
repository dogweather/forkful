---
aliases:
- /nl/lua/comparing-two-dates/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:33.257101-07:00
description: "Twee datums vergelijken betekent uitvogelen of een datum eerder, later\
  \ of hetzelfde is als een andere. Programmeurs doen dit om gebeurtenissen te volgen,\u2026"
lastmod: 2024-02-18 23:09:02.004901
model: gpt-4-0125-preview
summary: "Twee datums vergelijken betekent uitvogelen of een datum eerder, later of\
  \ hetzelfde is als een andere. Programmeurs doen dit om gebeurtenissen te volgen,\u2026"
title: Twee datums vergelijken
---

{{< edit_this_page >}}

## Wat & Waarom?

Twee datums vergelijken betekent uitvogelen of een datum eerder, later of hetzelfde is als een andere. Programmeurs doen dit om gebeurtenissen te volgen, taken te plannen, records te sorteren, en meer.

## Hoe te:

Lua heeft geen ingebouwde functies voor datumvergelijking, maar we kunnen de functie `os.time()` gebruiken om datums om te zetten naar een numeriek formaat (epochtijd) en ze dan vergelijken. Eitje.

```Lua
-- Datums omzetten naar epochtijd
local date1 = os.time({year=2023, month=4, day=1})
local date2 = os.time({year=2023, month=4, day=15})

-- De datums vergelijken
if date1 > date2 then
  print("Date1 is later dan Date2.")
elseif date1 < date2 then
  print("Date1 is eerder dan Date2.")
else
  print("Date1 is hetzelfde als Date2.")
end
```

Voorbeelduitvoer bij het uitvoeren met deze datums:

```
Date1 is eerder dan Date2.
```

## Diepere duik

Vroeger had Lua geen datumtype. Programmeurs vertrouwden op de `os.time()` functie voor datum-tijd operaties, die nog steeds wordt gebruikt. `os.time()` retourneert de tijd in seconden sinds het epoch (ook bekend als Unix-tijd, die begon op 1 januari 1970). Dit is handig omdat het datums omzet naar getallen, wat vergelijkingen vereenvoudigt.

Wat betreft alternatieven, je zou een aangepaste comparator voor datumtabellen kunnen schrijven, elk veld (jaar, maand, dag) handmatig kunnen vergelijken, of een externe datumlibrary zoals `LuaDate` kunnen gebruiken.

Wees bewust van tijdzones en de veranderingen van zomertijd wanneer je `os.time()` gebruikt. De functie gaat ervan uit dat je lokale tijd geeft, tenzij je anders aangeeft.

## Zie ook

- Lua 5.4 Referentiehandleiding: https://www.lua.org/manual/5.4/
- LuaDate, een datum- en tijdsmodule: https://github.com/Tieske/date
- Het begrijpen van Unix-timestamp: https://en.wikipedia.org/wiki/Unix_time
