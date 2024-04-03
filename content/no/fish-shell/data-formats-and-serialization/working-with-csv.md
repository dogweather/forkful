---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:41.087825-07:00
description: "\xC5 jobbe med CSV-filer (Comma Separated Values) inneb\xE6rer parsing,\
  \ manipulering og generering av data i et tabellformat som brukes bredt til datautveksling\u2026"
lastmod: '2024-03-13T22:44:41.249901-06:00'
model: gpt-4-0125-preview
summary: "\xC5 jobbe med CSV-filer (Comma Separated Values) inneb\xE6rer parsing,\
  \ manipulering og generering av data i et tabellformat som brukes bredt til datautveksling\
  \ mellom applikasjoner."
title: Arbeide med CSV
weight: 37
---

## Hvordan:
Fish Shell, i seg selv, har ikke innebygde funksjoner spesifikt designet for manipulering av CSV. Imidlertid kan du utnytte Unix-verktøy som `awk`, `sed`, og `cut` for grunnleggende operasjoner eller bruke spesialiserte verktøy som `csvkit` for mer avanserte oppgaver.

### Lese en CSV-fil og skrive ut den første kolonnen:
Bruker `cut` for å trekke ut den første kolonnen:
```fish
cut -d ',' -f1 data.csv
```
Eksempel på utdata:
```
Navn
Alice
Bob
```

### Filtrere CSV-rader basert på kolonneverdi:
Bruker `awk` for å finne rader hvor den andre kolonnen matcher "42":
```fish
awk -F, '$2 == "42" { print $0 }' data.csv
```
Eksempel på utdata:
```
Bob,42,London
```

### Modifisere en CSV-fil (f.eks., legge til en kolonne):
Bruker `awk` for å legge til en kolonne med en statisk verdi "NewColumn":
```fish
awk -F, 'BEGIN {OFS=","} {print $0,"NewColumn"}' data.csv > modified.csv
```
Eksempel på utdata i `modified.csv`:
```
Navn,Alder,By,NewColumn
Alice,30,New York,NewColumn
Bob,42,London,NewColumn
```

### Bruk av `csvkit` for mer avanserte operasjoner:
Først, sørg for at du har `csvkit` installert. Hvis ikke, installer det ved hjelp av pip: `pip install csvkit`.

**Konvertere en CSV-fil til JSON:**
```fish
csvjson data.csv > data.json
```
Eksempel på `data.json` utdata:
```json
[{"Navn":"Alice","Alder":"30","By":"New York"},{"Navn":"Bob","Alder":"42","By":"London"}]
```

**Filtrering med `csvkit` sitt `csvgrep`:**
```fish
csvgrep -c 2 -m 42 data.csv
```
Denne kommandoen replikerer filtreringsoppgaven, men ved å bruke `csvkit`, rettet mot kolonne 2 for verdien "42".

Konklusjonen er at selv om Fish Shell i seg selv kanskje ikke tilbyr direkte CSV-manipuleringsmuligheter, gir dens sømløse integrasjon med Unix-verktøy og tilgjengeligheten av verktøy som `csvkit` kraftige alternativer for arbeid med CSV-filer.
