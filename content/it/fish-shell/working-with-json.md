---
title:                "Lavorando con json"
html_title:           "Fish Shell: Lavorando con json"
simple_title:         "Lavorando con json"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## Cosa e perché?
Lavorare con JSON (JavaScript Object Notation) è una pratica comune per i programmatori perché è un formato di dati leggibile sia per gli esseri umani che per le macchine. È ampiamente utilizzato per lo scambio di informazioni tra diverse applicazioni o servizi web.

## Come fare:
Utilizzando Fish Shell, è possibile manipolare e interagire con i dati JSON in modo rapido e semplice. Ecco un esempio di come è possibile leggere un file JSON e accedere ai suoi valori:

```
Fish Shell
set json (cat file.json | fromjson)
echo $json.key
```

L'output sarà il valore associato alla chiave specificata all'interno del file JSON. Puoi anche utilizzare lo stesso approccio per scrivere un file JSON, sostituendo `fromjson` con `tojson`.

## Approfondimento:
JSON è stato creato nel 2001 da Douglas Crockford ed è diventato uno standard per lo scambio di dati grazie alla sua semplicità e flessibilità. Anche se è molto popolare, ci sono anche altri formati simili come YAML e XML. Fish Shell utilizza un'implementazione interna di JSON per gestire i dati, ma ci sono anche plugin disponibili per una maggiore flessibilità nell'utilizzo di JSON all'interno della shell.

## Vedi anche:
- [Documentazione di JSON su json.org](https://www.json.org/json-it.html)