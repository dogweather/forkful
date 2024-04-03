---
date: 2024-01-27 20:34:30.344907-07:00
description: "Hvordan: Lua har innebygd st\xF8tte for \xE5 generere tilfeldige tall\
  \ via `math.random`-funksjonen. Denne funksjonen kan brukes p\xE5 flere m\xE5ter,\
  \ avhengig av\u2026"
lastmod: '2024-03-13T22:44:40.925876-06:00'
model: gpt-4-0125-preview
summary: "Lua har innebygd st\xF8tte for \xE5 generere tilfeldige tall via `math.random`-funksjonen."
title: Generering av tilfeldige tall
weight: 12
---

## Hvordan:
Lua har innebygd støtte for å generere tilfeldige tall via `math.random`-funksjonen. Denne funksjonen kan brukes på flere måter, avhengig av ønsket utdata:

1. **Generere et tilfeldig flyttall mellom 0 og 1:**

```Lua
print(math.random())
```

Eksempel på utdata kan være `0.13117647051304`. Hver kjøring produserer en annen verdi.

2. **Generere et tilfeldig heltall innenfor et spesifisert område:**

For å produsere et tilfeldig heltall mellom to grenser, inkludert, må du først sette frøet ved hjelp av `math.randomseed(os.time())` for variabilitet, deretter kalle `math.random` med to argumenter:

```Lua
math.randomseed(os.time())
print(math.random(1, 10)) -- Genererer et tilfeldig heltall mellom 1 og 10
```

Eksempel på utdata kan være `7`. Igjen, vil utdata variere med hver utførelse.

Det er avgjørende å sette frøet med `math.randomseed` fordi uten det, kunne `math.random` generere den samme sekvensen av tall hver gang et program kjøres. Å vanligvis så med gjeldende tid, `os.time()`, sikrer forskjellige sekvenser per utførelse.

## Dypdykk
Mekanismen som ligger til grunn for generering av tilfeldige tall i Lua (og de fleste programmeringsspråk) er ikke virkelig tilfeldig, men pseudotilfeldig, generert av en algoritme. Disse pseudotilfeldige tallgeneratorene (PRNG-er) er deterministiske og krever en startverdi for å begynne sekvensen av tallgenerering. Valget av såing er avgjørende for kvaliteten på tilfeldigheten, noe som er grunnen til at bruken av gjeldende tid er en vanlig praksis.

Historisk har Lua's muligheter for generering av tilfeldige tall utviklet seg. Tidligere versjoner stolte på C-standardsbibliotekets `rand()`-funksjon, som varierte i kvalitet og ytelse på tvers av implementasjoner. Den nåværende versjonen av Lua forbedrer dette ved muligens å bruke mer robuste mekanismer avhengig av den underliggende plattformen, og tilbyr større konsistens og nyttighet i generering av tilfeldige tall.

For prosjekter som krever kryptografisk-nivå tilfeldighet, kan den innebygde Lua-funksjonaliteten være utilstrekkelig på grunn av den deterministiske naturen til PRNG-er. I slike tilfeller vender programmerere ofte til eksterne biblioteker eller systemspesifikke APIer som kan tilby ikke-deterministiske tilfeldige tall som er egnet for høysikkerhetsapplikasjoner.
