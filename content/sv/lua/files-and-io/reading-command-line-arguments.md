---
date: 2024-01-20 17:56:41.370092-07:00
description: "Kommandoradsargument l\xE5ter anv\xE4ndare ge instruktioner till ett\
  \ program n\xE4r de startar det. Programmerare anv\xE4nder detta f\xF6r att g\xF6\
  ra program flexibla och\u2026"
lastmod: '2024-03-13T22:44:38.054316-06:00'
model: gpt-4-1106-preview
summary: "Kommandoradsargument l\xE5ter anv\xE4ndare ge instruktioner till ett program\
  \ n\xE4r de startar det."
title: "L\xE4sa in kommandoradsargument"
weight: 23
---

## How to:
Läs in argument från kommandoraden i Lua så här:

```Lua
-- Spara filen som hello.lua
-- Kör programmet från terminalen: lua hello.lua hej värld

for i = 1, #arg do
  print("Argument " .. i .. ": " .. arg[i])
end
```

Kört med `lua hello.lua hej värld` ger:

```
Argument 1: hej
Argument 2: värld
```

## Deep Dive
Lua's innebyggda `arg`-tablett innehåller argumenten som skickas till ett script. De flesta skriptspråk har liknande mekanismer, men Lua skiljer sig i att `arg[0]` är scriptets namn, och `arg[n]` (där n är ett positivt heltal) innehåller de faktiska argumenten som startar från index 1.

Historiskt sett kommer praxisen att använda kommandoradsargument från de tidigaste dagarna av programmering där interaktion med program var begränsad till textbaserade terminaler.

Det finns alternativ för att hantera kommandoradsargument som mer avancerade bibliotek som `Penlight` eller `lua-argparse`, som tillhandahåller funktioner för att definiera och bearbeta kommandoradsflaggor och -optioner på ett mer sofistikerat sätt.

Implementeringsdetaljer i Lua är rakt på sak. Skript kan löpa utan att explicit hantera `arg` om de inte behöver kommandoradsargument, vilket gör det valfritt men lättillgängligt.

## See Also
- [Lua 5.4 Reference Manual (arg)](https://www.lua.org/manual/5.4/manual.html#6.1)
- [Stack Overflow: How to parse command-line arguments in Lua](https://stackoverflow.com/questions/4537269/how-to-parse-command-line-arguments-in-lua)
- [GitHub: lua-argparse](https://github.com/mpeterv/argparse)
