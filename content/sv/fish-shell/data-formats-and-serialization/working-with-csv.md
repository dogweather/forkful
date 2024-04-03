---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:43.308981-07:00
description: "Hur man g\xF6r: Fish Shell har i sig inga inbyggda funktioner som specifikt\
  \ \xE4r avsedda f\xF6r manipulation av CSV. Dock kan du utnyttja Unix-verktyg som\
  \ `awk`,\u2026"
lastmod: '2024-03-13T22:44:38.360642-06:00'
model: gpt-4-0125-preview
summary: "Fish Shell har i sig inga inbyggda funktioner som specifikt \xE4r avsedda\
  \ f\xF6r manipulation av CSV."
title: Arbeta med CSV
weight: 37
---

## Hur man gör:
Fish Shell har i sig inga inbyggda funktioner som specifikt är avsedda för manipulation av CSV. Dock kan du utnyttja Unix-verktyg som `awk`, `sed`, och `cut` för grundläggande operationer eller använda specialiserade verktyg som `csvkit` för mer avancerade uppgifter.

### Läsa en CSV-fil och skriva ut den första kolumnen:
Använd `cut` för att extrahera den första kolumnen:
```fish
cut -d ',' -f1 data.csv
```
Exempel på utdata:
```
Namn
Alice
Bob
```

### Filtrera CSV-rader baserat på kolumnvärde:
Använd `awk` för att hitta rader där den andra kolumnen matchar "42":
```fish
awk -F, '$2 == "42" { print $0 }' data.csv
```
Exempel på utdata:
```
Bob,42,London
```

### Modifiera en CSV-fil (t.ex. lägga till en kolumn):
Använd `awk` för att lägga till en kolumn med ett statiskt värde "NewColumn":
```fish
awk -F, 'BEGIN {OFS=","} {print $0,"NewColumn"}' data.csv > modified.csv
```
Exempel på utdata i `modified.csv`:
```
Namn,Ålder,Stad,NewColumn
Alice,30,New York,NewColumn
Bob,42,London,NewColumn
```

### Använda `csvkit` för mer avancerade operationer:
Se till att du först har `csvkit` installerat. Om inte, installera det med pip: `pip install csvkit`.

**Konvertera en CSV-fil till JSON:**
```fish
csvjson data.csv > data.json
```
Exempel på `data.json`-utdata:
```json
[{"Namn":"Alice","Ålder":"30","Stad":"New York"},{"Namn":"Bob","Ålder":"42","Stad":"London"}]
```

**Filtrera med `csvkit`'s `csvgrep`:**
```fish
csvgrep -c 2 -m 42 data.csv
```
Detta kommando replikerar filtreringsuppgiften men använder `csvkit`, med fokus på kolumn 2 för värdet "42".

Sammanfattningsvis, även om Fish Shell i sig kanske inte erbjuder direkt hantering av CSV-filer, ger dess sömlösa integration med Unix-verktyg och tillgången på verktyg som `csvkit` kraftfulla alternativ för arbete med CSV-filer.
