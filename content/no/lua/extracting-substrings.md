---
title:                "Ekstrahering av delstrenger"
html_title:           "Lua: Ekstrahering av delstrenger"
simple_title:         "Ekstrahering av delstrenger"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Utvinning av delstrenger, også kalt delstrengmanipulering, er en viktig del av programmering. Det innebærer å trekke ut en del av en eksisterende tekststreng og bruke den som en separat del av koden. Dette kan være nyttig for å matche og filtrere data eller for å formatere informasjon på en lesbar måte.

## Slik gjør du det:
Se for deg at du har en tekststreng med navnet "Lua-programmering". Hvis du ønsker å ekstrahere navnet "Lua", kan du bruke følgende kode:

```Lua
-- Lag en tekststreng
tekststreng = "Lua-programmering"

-- Ekstraher delstreng ved å angi start og slutt indekser
delstreng = string.sub(tekststreng, 1, 3)

-- Skriv ut delstreng
print(delstreng)
```
Dette vil gi følgende output:

```
Lua
```

Du kan også bruke delstrekksposisjonen som en variabel og kombinere den med andre tekststrenger som denne:

```Lua
-- Lag en tekststreng
navn = "Sven"

-- Kombiner navnet med en annen tekststreng
tekststreng = navn .. "-programmering"

-- Ekstraher delstreng ved å angi start og slutt indekser
delstreng = string.sub(tekststreng, 1, 3)

-- Skriv ut delstreng
print(delstreng)
```

Dette vil gi følgende output:

```
Sve
```

## Dypdykk:
Utvinning av delstrenger har vært en del av programmering siden de tidligste språkene ble utviklet, og det finnes flere ulike måter å gjøre det på. Noen programmeringsspråk har egne funksjoner for å ekstrahere delstrenger, mens andre krever at man spesifiserer start og slutt indekser manuelt. I Lua, kan man også bruke negative indekser, som teller fra slutten av tekststrengen, for eksempel hvis man ønsker å ekstrahere de siste tre bokstavene.

## Se også:
http://www.lua.org/manual/5.4/manual.html#6.4.7