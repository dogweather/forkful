---
title:                "Sammenkobling av strenger"
html_title:           "Lua: Sammenkobling av strenger"
simple_title:         "Sammenkobling av strenger"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Sammenkobling av strenger refererer til å kombinere to eller flere strenger til én. Dette er en vanlig praksis blant programmerere for å lage lengre tekststrenger eller til å bygge opp mer dynamiske meldinger.

## Hvordan gjør man det:
I Lua kan man enkelt sammenkoble strenger ved å bruke “..” operatøren. Dette er også kjent som konkatenere. La oss se på et eksempel:

```Lua
local fornavn = "Per"
local etternavn = "Hansen"
local fulltNavn = fornavn .. " " .. etternavn
print(fulltNavn)
```

Dette vil gi følgende output: `Per Hansen`.
Vi kan også kombinere tall og strenger:

```Lua
local tall = 42
local tekst = "Svaret på alt er:"
local komplettMelding = tekst .. " " .. tall
print(komplettMelding)
```
Dette vil gi output: `Svaret på alt er: 42`.

## Dypdykk:
Konkatenere strenger er ikke en unik funksjon i Lua, dette er noe som er tilgjengelig i de fleste programmeringsspråk. I tillegg til å bruke “..” operatøren, kan man også bruke `string.format()` funksjonen for å sammenkoble strenger. Dette kan være mer nyttig hvis man trenger å formatere strenger slik som setninger eller tall med desimaler. Man kan også bruke `table.concat()` funksjonen for å sammenkoble elementer i en tabell.  

## Se også:
* Dokumentasjon for Lua sammenkobling av strenger: https://www.lua.org/manual/5.3/manual.html#3.4.6
* Andre måter å håndtere tekststrenger i Lua: https://www.lua.org/pil/20.3.html