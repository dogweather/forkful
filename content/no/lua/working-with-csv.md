---
title:                "Å arbeide med CSV"
html_title:           "Lua: Å arbeide med CSV"
simple_title:         "Å arbeide med CSV"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/working-with-csv.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å jobbe med CSV-filer (Comma Separated Values) innebærer å lese og skrive data fra og til en strukturert tekstfil ved hjelp av komma som skille mellom verdier. Dette er en viktig oppgave for programmere for å kunne håndtere store mengder data på en effektiv måte.

## Hvordan:
For å jobbe med CSV-filer i Lua, kan du bruke biblioteket "csv". Ved hjelp av funksjoner som "csv.open" og "csv.parse", kan du enkelt lese og behandle data fra en CSV-fil.

```Lua
local csv = require("csv")

-- Åpne en CSV-fil og lese inn data
local data = csv.open("eksampel.csv", {headers=true})

-- Itererer gjennom alle rader og skriver ut verdiene
for row in data:lines() do
  print("Navn: " .. row.name .. ", Alder: " .. row.age)
end
```
Output:
```
Navn: Ola, Alder: 25
Navn: Kari, Alder: 30
```

## Dypdykk:
CSV ble opprinnelig utviklet på 1970-tallet som en enkel måte å lagre og dele data mellom ulike programmer. Alternativer til CSV inkluderer XML, JSON og SQL-databaser. I Lua er det også mulig å jobbe med CSV-filer ved å bruke standardbiblioteket io og string-funksjoner som "split" og "gsub".

## Se også:
- [Generell informasjon om CSV-filer](https://no.wikipedia.org/wiki/CSV)