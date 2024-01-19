---
title:                "Sjekker om en mappe eksisterer"
html_title:           "Lua: Sjekker om en mappe eksisterer"
simple_title:         "Sjekker om en mappe eksisterer"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sjekke om en katalog eksisterer er det å verifisere tilstedeværelsen av et spesifikt katalognavn innen et bestemt filsystem. Programmerere gjør dette for å unngå feil når de prøver å få tilgang til kataloger som kanskje ikke eksisterer.

## Hvordan gjør man dette:
Her er en grunnleggende kodebit på Lua for å sjekke om en katalog eksisterer:

```Lua
local lfs = require('lfs')

function directory_exists(path)
  return lfs.attributes(path, 'mode') == 'directory'
end

print(directory_exists("./example_directory"))
```
Utførelsen av denne koden vil returnere `true` hvis 'example_directory' eksisterer, og `false` hvis den ikke gjør det.

## Dypdykk:
Historisk sett har det alltid vært behov for metoder for å sjekke om en katalog eksisterer innen programmering. Det er fordi at ønsket tilgang til en ikke-eksisterende katalog ville være katastrofalt for mange programmer. 

Alternativer til `lfs.attributes` funksjonen finnes, som bruk av `os.execute` funksjonen med en shellkommando. Dette er imidlertid ikke anbefalt da det er plattformavhengig og potensielt usikker. 

Implementeringsdetaljene for å sjekke om en katalog eksisterer i Lua stammer fra `lfs.attributes` funksjonen som er en del av LuaFileSystem-biblioteket. Når det gis en sti og 'mode' som argumenter, returnerer det modusattributtet til den gitte stien, noe som muliggjør verifikasjon av om stien er en katalog.

## Se også:
1. [LuaFileSystem Dokumentasjon](http://keplerproject.github.io/luafilesystem/manual.html)
2. [Lua Programmeringsguide](http://www.lua.org/pil/contents.html)
3. ['lfs.attributes' funksjonsdetaljer på Stack Overflow](https://stackoverflow.com/questions/1340230/check-if-directory-exists-in-lua)
4. [Sikker kodepraksis for Lua på Lua-Users](http://lua-users.org/wiki/SecureProgramming)