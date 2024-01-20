---
title:                "Lese kommandolinjeargumenter"
html_title:           "Arduino: Lese kommandolinjeargumenter"
simple_title:         "Lese kommandolinjeargumenter"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å lese kommandolinjeargumenter er prosessen der programmer mottar data fra brukeren gjennom kommandolinjen. Programmerere gjør dette for å kontrollere programflyten og manipulere data uten brukergrensesnitt.

## Hvordan:
Hvis du skriver et Lua-program som trenger å lese kommandolinjeargumenter, f.eks. filnavn eller innstillinger, kan du bruke den innebygde globale tabellen `arg`.

```Lua
for i, v in ipairs(arg) do
  print("arg[" .. i .. "]", v)
end
```
Hvis du kjører programmet ditt med `lua test.lua arg1 arg2 arg3`, vil dette skrive ut:

```Lua
arg[0] test.lua
arg[1] arg1
arg[2] arg2
arg[3] arg3
```

## Dyp Dykk
Historisk har handlingen med å lese kommandolinjeargumenter vært et viktig aspekt for programmering og scriptspråk. Før grafiske brukergrensesnitt var kommandolinjen det primære brukergrensesnittet, og selv i dag er det kritisk for skripting og automatisering.

Alternativt, i noen tilfelle, kan man bruke en kommando-linjeargument-parser, som er litt mer komplisert, men gir større fleksibilitet og mer avanserte funksjoner.

Detaljer om implementering: I Lua er alle kommandolinjeargumenter lagret i tabellen `arg`. `arg[0]` er alltid navnet på skriptet, og `arg[n]` (n> 0) er argumentene.

## Se Også
1. Lua 5.4 Referanse Manual: https://www.lua.org/manual/5.4/
2. Lua-Users Wiki, Command Line Arguments: http://lua-users.org/wiki/CommandLineArguments
3. Penlight Lua, Modul for behandling av argumenter: https://github.com/stevedonovan/Penlight