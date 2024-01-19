---
title:                "Opprette en midlertidig fil"
html_title:           "C#: Opprette en midlertidig fil"
simple_title:         "Opprette en midlertidig fil"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Oppretting av en midlertidig fil er en handling som oppretter en fil med tilfeldige data for kortvarig bruk. Programmerere gjør dette for å lagre data midlertidig, for eksempel for sortering eller buffring.

## Hvordan:

Her er en grunnleggende kode som handler om hvordan du oppretter en midlertidig fil i Lua:

```Lua
local tmp = os.tmpname()

-- Skriv data til den midlertidige filen
local f = io.open(tmp, 'w')
f:write('Hello, Norway!')
f:close()

-- Les data fra den midlertidige filen
local f = io.open(tmp, 'r')
print(f:read("*all")) -- utganger: Hello, Norway!
f:close()
```

## Dypdykk

Historisk sett har midlertidige filer alltid vært viktige i programmering for lagring av midlertidige data. De brukes ofte under prosessering av store datamengder som ikke passer i minnet, eller når dataene må deles mellom flere programmer.

Alternativt til å opprette midlertidige filer, kan programmerere bruke datastrukturer i minnet dersom dataene er små nok, eller databaseservere for mer robust og sikker datalagring mellom operasjoner.

Når du bruker `os.tmpname()` funksjonen i Lua, lager Lua et unikt filnavn men ikke selve filen. Du må åpne filen selv for å bruke den. Funksjonen `io.open()` brukes til å åpne filen, enten for skriving med 'w' parametre eller for lesing med 'r' parametre. 

## Se Også

For mer informasjon om filhåndtering i Lua, sjekk ut følgende lenker:

1. Offisiell Lua Dokumentasjon: https://www.lua.org/manual/5.4/
2. Tutorial for File I/O i Lua: https://www.tutorialspoint.com/lua/lua_file_io.htm
3. Lua brukerveiledning for 'io' biblioteket: https://www.lua.org/pil/21.2.1.html