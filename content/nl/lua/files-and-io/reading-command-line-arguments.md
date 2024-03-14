---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:11.817112-07:00
description: "Commandoregelargumenten lezen betekent het oppakken van de extra stukjes\
  \ die je na de naam van je script in de console typt. Programmeurs doen dit om het\u2026"
lastmod: '2024-03-13T22:44:50.952703-06:00'
model: gpt-4-0125-preview
summary: "Commandoregelargumenten lezen betekent het oppakken van de extra stukjes\
  \ die je na de naam van je script in de console typt. Programmeurs doen dit om het\u2026"
title: Commandoregelargumenten lezen
---

{{< edit_this_page >}}

## Wat & Waarom?

Commandoregelargumenten lezen betekent het oppakken van de extra stukjes die je na de naam van je script in de console typt. Programmeurs doen dit om het gedrag van een programma aan te passen zonder de code te wijzigen, zoals het kiezen van een bestand om te openen of het instellen van de uitvoerigheid van output.

## Hoe:

Hier is de kern van het grijpen van die argumenten in Lua:

```Lua
-- Sla dit op als 'greet.lua'
local name = arg[1] -- arg[1] is het eerste commandoregelargument
print("Hallo, " .. (name or "vreemdeling") .. "!")
```

Start de terminal en run het:

```
$ lua greet.lua LuaLearner
Hallo, LuaLearner!
```

Geen naam? Geen probleem:

```
$ lua greet.lua
Hallo, vreemdeling!
```

## Diepgaand

Lua houdt het eenvoudig met de globale `arg` tabel. Historisch gezien lezen mensen al commandoregelargumenten in programmering sinds het begin der tijden (nou ja, sinds de geboorte van UNIX, in ieder geval). Het is een basis van aanpassing.

In Lua is `arg` een array met alle commandoregelheerlijkheden. `arg[0]` is de scriptnaam, en vanaf `arg[1]` zijn het de daadwerkelijke argumenten. Je kunt ze allemaal oppakken met een lus als je je chique voelt:

```Lua
for i = 1, #arg do
  print("Argument " .. i .. ": " .. arg[i])
end
```

Alternatieven? Zeker, er zijn bibliotheken voor gesofisticeerde argumentenanalyse (zoals `Penlight`), maar in veel gevallen doet `arg` de truc zonder gedoe.

Wat implementatiedetails betreft, onthoud dat Lua's arrays gebaseerd zijn op 1 (ze beginnen te tellen bij 1), niet 0 zoals veel andere talen. Dat is waarom `arg[1]` het eerste argument is en niet `arg[0]`.

## Zie Ook

Voor degenen die meer willen, hier is wat extra voedsel voor gedachten:

- Lua 5.4 Referentiehandleiding over de `arg` tabel: https://www.lua.org/manual/5.4/manual.html#6.1
- "Programmeren in Lua" (4e editie) voor een solide grip op de basis van Lua: https://www.lua.org/pil/contents.html
- Penlight, een Lua hulpprogramma bibliotheek met uitgebreide argumentenanalyse: https://github.com/lunarmodules/Penlight
