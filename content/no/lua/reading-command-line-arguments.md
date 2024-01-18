---
title:                "Lese kommandolinjeargumenter"
html_title:           "Lua: Lese kommandolinjeargumenter"
simple_title:         "Lese kommandolinjeargumenter"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Å lese kommandolinjeargumenter er en måte for programmerere å få innspill fra brukere når et program kjøres. Dette gjør det mulig å gjøre programmet mer interaktivt og tilpasse det til brukerens behov.

# Hvordan:
I Lua kan vi lese kommandolinjeargumenter ved hjelp av standardbiblioteket "arg". Dette biblioteket har en funksjon kalt "arg" som lar oss få tilgang til de ulike argumentene som er gitt ved å kjøre programmet.
```Lua
-- Eksempel på å sjekke om et bestemt argument er gitt:
if arg[1] == "-h" then
    print("Velkommen til hjelpesiden!")
end
```
Dette vil sjekke om det første argumentet gitt er "-h" (for hjelp) og gi en beskjed til brukeren om det er tilfellet. Hvis programmet ble kjørt med "lua program.lua -h", vil det skrives ut "Velkommen til hjelpesiden!".

# Dykk dypere:
Historisk sett, var kommandolinjeargumenter den eneste måten å samhandle med datamaskiner på. Fordi det var begrensede grafiske grensesnitt, var det vanlig å kjøre programmer fra kommandolinjen ved å gi ulike argumenter som bestemte hvordan programmet skulle kjøre.

Alternativt, kan man også samhandle med brukeren ved hjelp av brukerinput underveis i programmet. Dette gjøres vanligvis med funksjoner som "io.read()" i Lua.

Implementasjonsmessig, tar "arg" biblioteket inn kommandolinjeargumentene og lagrer de i en tabell. Dette gjør det enkelt å få tilgang til og behandle argumentene i programmet.

# Se også:
For mer informasjon om å lese kommandolinjeargumenter i Lua, kan du se dokumentasjonen til Lua sin "arg" funksjon: https://www.lua.org/manual/5.4/manual.html#6.9. Også, her er en artikkel som gir en god forklaring på ulike måter å samhandle med brukeren i Lua: https://learnxinyminutes.com/docs/no-no/lua-no/.