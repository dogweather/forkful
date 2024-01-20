---
title:                "Utdrag av understrenger"
html_title:           "Bash: Utdrag av understrenger"
simple_title:         "Utdrag av understrenger"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å ekstrahere delstrenger er en prosess hvor vi henter en del av en større streng, bestemt av sin start og slutt posisjon. Dette er praktisk når vi skal manipulere spesifikke deler av tekst i vår kode.

## Hvordan Gjøre Dette
I Lua, kan vi bruke `string.sub` funksjonen for å hente delstrenger. Se dette eksempelet:

```Lua
tekst = "Hei Verden!" 
print(string.sub(tekst, 1, 3)) 
```
Output vil være:

```
Hei
```
Her henter vi karakterene fra posisjon 1 til 3 fra `tekst` strengen.

## Dykket Ned
1. Historisk Kontekst: Lua, designet i 1993 av et team i Brasil, er kjent for sin enkelthet i manipulering av strenger. `string.sub` er en funksjon som har vært med fra starten.

2. Alternativer: En annen måte er å bruke `string.match`, som kan brukes med mønstre for å hente ut mer komplekse delstrenger.

```Lua
tekst = "Hei Verden!" 
print(string.match(tekst, "%a+")) 
```
Output:

```
Hei
```
Her henter vi ut det første ordet i strengen.

3. Implementerings Detaljer: `string.sub` mottar tre argumenter: strengen å manipulere, startposisjon, og sluttposisjon. Indeksering i Lua starter fra 1, ikke 0.

## Se Også
For mer informasjon, se disse relaterte kildene:
1. Offisiell Lua dokumentasjon om strenger: https://www.lua.org/manual/5.4/manual.html#6.4
2. Lua Brukerveiledning om `string.sub`: http://lua-users.org/wiki/StringLibraryTutorial
3. Diskusjon på StackOverflow om ekstrahering av delstrenger: https://stackoverflow.com/questions/20777089/lua-extract-substring-from-string