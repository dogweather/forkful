---
title:                "Lese en tekstfil"
html_title:           "C#: Lese en tekstfil"
simple_title:         "Lese en tekstfil"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Lesing av tekstfiler i Lua

## Hva & Hvorfor?

Å lese en tekstfil er prosessen hvor programmer henter data fra en lagret fil på ditt system. Dette gjøres for å bruke eller manipulere disse dataene i programmet ditt.

## Slik Gjør Du:

Her er et enkelt eksempel hvordan du leser en tekstfil i Lua:

```Lua
fil = io.open("test.txt", "r")

io.input(fil)

print(io.read())

io.close(file)
```

Hvis din "test.txt" fil inneholder "Hei, Verden!", så vil dette være output:

```Lua
Hei, Verden!
```

## Dypdykk

Historisk har filoperasjoner alltid vært en viktig del av programmering. I Lua, la "io"-biblioteket programmerere lese og skrive data på filer ved å forenkle interaksjonen med systemfiler. 

Alternativt kan du bruke "*file:read()*" for å lese linje-for-linje, som kan være nyttig for større filer. 

```Lua
fil = io.open("test.txt", "r")

for linje in fil:lines() do
  print(linje)
end

fil:close()
```

Når det gjelder ytterligere detaljer om implementering, filoperasjoner skal alltid følge opp med fil:close() for å frigjøre systemressurser.

## Se Også

For mer informasjon om filhåndtering i Lua, sjekk ut disse nyttige kildene:

1. Offisiell Lua IO Library Tutorial: [Link](http://www.lua.org/pil/21.2.1.html)
2. Tutorial på å lese og skrive filer i Lua: [Link](https://www.tutorialspoint.com/lua/lua_file_io.htm)