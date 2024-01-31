---
title:                "Skrive til standardfeil"
date:                  2024-01-19
html_title:           "Arduino: Skrive til standardfeil"
simple_title:         "Skrive til standardfeil"

category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Skriving til standard error (stderr) lar programmer rapportere feil uten å forstyrre normal output. Programmerere bruker det for å skille vanlige data fra feilsøkingsinformasjon.

## Hvordan gjøre det:
```Lua
-- Skriver en enkel feilmelding til stderr
io.stderr:write("En feil oppstod\n")

-- Håndtere et filåpningsproblem
local file, err = io.open("ikkeeksisterendefil.txt", "r")
if not file then
  io.stderr:write("Feil ved åpning av fil: " .. err .. "\n")
end
```
Sample output:
```
En feil oppstod
Feil ved åpning av fil: ikkeeksisterendefil.txt: No such file or directory
```

## Dypdykk
Historisk sett er skille mellom standard output (stdout) og stderr en Unix-konvensjon, designet for å la brukere omdirigere dem separat. I Lua utføres det via `io` biblioteket, som tilbyr lavnivå tilgang til filhåndtering. Et alternativ er å bruke `os.execute` med en shell-kommando for å omdirigere stderr. Implementering i Lua er enkel og krever ikke eksterne biblioteker.

## Se Også
- Lua 5.4 reference manual om `io` biblioteket: http://www.lua.org/manual/5.4/manual.html#6.8
- Lua-users wiki om File I/O: http://lua-users.org/wiki/FileInputOutput
- Stack Overflow for praktiske bruksmønstre og problemløsning relatert til stderr i Lua: https://stackoverflow.com/search?q=lua+stderr
