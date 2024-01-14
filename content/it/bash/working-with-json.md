---
title:                "Bash: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/working-with-json.md"
---

{{< edit_this_page >}}

## Perché lavorare con JSON
Se sei un programmatore, probabilmente hai sentito parlare di JSON. Ma perché dovresti impegnarti a lavorare con questa struttura dati? In poche parole, JSON è una formatta semplice e leggibile per scambiare dati tra applicazioni e server web. È ampiamente utilizzato e di facile comprensione, quindi è un must per ogni programmatore.

## Come fare
Per lavorare con JSON in Bash, è necessario utilizzare il comando `jq`, un potente strumento per estrarre, manipolare e analizzare i dati JSON. Controlliamo un semplice esempio di come utilizzare `jq` per estrarre i dati da un file JSON:

```
Bash jq '.nome' file.json
```

Nell'esempio sopra, `jq` estrae il valore della chiave "nome" dal file JSON specificato e lo stampa nel terminale. Puoi anche utilizzare `jq` per filtrare, combinare e manipolare i dati JSON in modi più complessi. Ci sono molte risorse online che possono aiutarti a imparare a utilizzare `jq` al massimo delle sue capacità.

## Approfondimenti
Se vuoi perfezionare le tue conoscenze su JSON e come utilizzarlo in Bash, ecco alcuni suggerimenti aggiuntivi:

- Leggi il manuale di `jq` per scoprire tutte le sue funzioni e opzioni disponibili.
- Esplora come utilizzare `jq` insieme ad altre potenti funzionalità di Bash, come i loop, le funzioni e le variabili.
- Pratica con esempi sempre più complessi per migliorare la tua comprensione e abilità nell'utilizzo di `jq` con JSON.

## Vedi anche
- [Guida veloce a JSON](https://json.org/json-it.html)
- [Documentazione jq](https://stedolan.github.io/jq/)
- [Corso interattivo sui dati JSON con Bash e jq](https://www.linkedin.com/learning/working-with-json-data-using-bash-and-jq)