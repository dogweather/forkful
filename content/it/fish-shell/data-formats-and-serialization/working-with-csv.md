---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:44.567942-07:00
description: "Lavorare con file CSV (Comma Separated Values, Valori Separati da Virgola)\
  \ comporta l'analisi, manipolazione e generazione di dati in un formato tabellare\u2026"
lastmod: '2024-03-13T22:44:43.882859-06:00'
model: gpt-4-0125-preview
summary: "Lavorare con file CSV (Comma Separated Values, Valori Separati da Virgola)\
  \ comporta l'analisi, manipolazione e generazione di dati in un formato tabellare\u2026"
title: Lavorare con i CSV
weight: 37
---

## Cosa e perché?

Lavorare con file CSV (Comma Separated Values, Valori Separati da Virgola) comporta l'analisi, manipolazione e generazione di dati in un formato tabellare ampiamente utilizzato per lo scambio di dati tra applicazioni. I programmatori eseguono queste operazioni per elaborare ed analizzare i dati in modo efficiente, automatizzare compiti o integrarsi con altri sistemi.

## Come fare:

Fish Shell, di per sé, non dispone di funzioni integrate specificamente progettate per la manipolazione di CSV. Tuttavia, è possibile sfruttare utility Unix come `awk`, `sed` e `cut` per operazioni di base o utilizzare strumenti specializzati come `csvkit` per compiti più avanzati.

### Leggere un file CSV e stampare la prima colonna:
Usando `cut` per estrarre la prima colonna:
```fish
cut -d ',' -f1 data.csv
```
Output di esempio:
```
Nome
Alice
Bob
```

### Filtrare le righe CSV in base al valore di una colonna:
Usando `awk` per trovare le righe dove la seconda colonna corrisponde a "42":
```fish
awk -F, '$2 == "42" { print $0 }' data.csv
```
Output di esempio:
```
Bob,42,Londra
```

### Modificare un file CSV (es., aggiungere una colonna):
Usando `awk` per aggiungere una colonna con un valore statico "NuovaColonna":
```fish
awk -F, 'BEGIN {OFS=","} {print $0,"NuovaColonna"}' data.csv > modificato.csv
```
Output di esempio in `modificato.csv`:
```
Nome,Età,Città,NuovaColonna
Alice,30,New York,NuovaColonna
Bob,42,Londra,NuovaColonna
```

### Utilizzare `csvkit` per operazioni più avanzate:
Prima, assicurati di avere `csvkit` installato. Se non lo è, installalo usando pip: `pip install csvkit`.

**Convertire un file CSV in JSON:**
```fish
csvjson data.csv > data.json
```
Output di esempio `data.json`:
```json
[{"Nome":"Alice","Età":"30","Città":"New York"},{"Nome":"Bob","Età":"42","Città":"Londra"}]
```

**Filtrare con `csvgrep` di `csvkit`:**
```fish
csvgrep -c 2 -m 42 data.csv
```
Questo comando replica il compito di filtraggio ma usando `csvkit`, prendendo di mira la colonna 2 per il valore "42".

In conclusione, sebbene Fish Shell in sé non offra capacità dirette di manipolazione dei CSV, la sua integrazione senza problemi con utility Unix e la disponibilità di strumenti come `csvkit` forniscono potenti opzioni per lavorare con i file CSV.
