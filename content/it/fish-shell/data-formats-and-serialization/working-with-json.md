---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:35.339363-07:00
description: "Lavorare con JSON in Fish Shell comporta l'analisi e la generazione\
  \ di dati JSON, un compito comune per la configurazione delle applicazioni,\u2026"
lastmod: '2024-02-25T18:49:41.722755-07:00'
model: gpt-4-0125-preview
summary: "Lavorare con JSON in Fish Shell comporta l'analisi e la generazione di dati\
  \ JSON, un compito comune per la configurazione delle applicazioni,\u2026"
title: Lavorare con JSON
---

{{< edit_this_page >}}

## Cosa e perché?

Lavorare con JSON in Fish Shell comporta l'analisi e la generazione di dati JSON, un compito comune per la configurazione delle applicazioni, l'interazione con API e la razionalizzazione dei flussi di lavoro da linea di comando. Data l'onnipresenza del JSON nello sviluppo web e applicativo, padroneggiarne la manipolazione direttamente nella shell può significativamente accrescere l'automazione e l'efficienza nella gestione dei dati per i programmatori.

## Come fare:

Fish Shell, di per sé, non dispone di utilità integrate per l'analisi e la generazione di JSON. Tuttavia, si integra perfettamente con strumenti di terze parti come `jq` per l'elaborazione di JSON. `jq` è un processore di JSON da linea di comando potente e versatile che ti consente di dividere, filtrare, mappare e trasformare dati strutturati con un linguaggio semplice ed espressivo.

### Analizzare JSON con jq
Per analizzare un file JSON ed estrarre dati utilizzando `jq`:

```fish
# Supponendo di avere un file JSON denominato 'data.json' con contenuto: {"name":"Fish Shell","version":"3.4.0"}
cat data.json | jq '.name'
# Esempio di output
"Fish Shell"
```

### Generare JSON con jq
Creare contenuto JSON a partire da variabili o output della shell:

```fish
# Creare oggetto JSON dalle variabili
set name "Fish Shell"
set version "3.4.0"
jq -n --arg name "$name" --arg version "$version" '{name: $name, version: $version}'
# Esempio di output
{
  "name": "Fish Shell",
  "version": "3.4.0"
}
```

### Filtrare Collezioni JSON
Supponiamo di avere un array JSON di oggetti in un file denominato `versions.json`:
```json
[
  {"version": "3.1.2", "stable": true},
  {"version": "3.2.0", "stable": false},
  {"version": "3.4.0", "stable": true}
]
```
Per filtrare questo array solo per le versioni stabili:

```fish
cat versions.json | jq '.[] | select(.stable == true) | .version'
# Esempio di output
"3.1.2"
"3.4.0"
```

Gli esempi forniti dimostrano la potenza dell'integrazione di `jq` con Fish Shell per le operazioni su JSON. Sfruttare tali strumenti arricchisce l'esperienza della shell, rendendola un ambiente formidabile per la gestione dei formati di dati moderni.
