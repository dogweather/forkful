---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:35.622486-07:00
description: "Lavorare con file CSV (Valori Separati da Virgola) in Bash consiste\
  \ nel processare e manipolare dati tabellari memorizzati in formato di testo semplice.\u2026"
lastmod: '2024-03-13T22:44:43.621977-06:00'
model: gpt-4-0125-preview
summary: "Lavorare con file CSV (Valori Separati da Virgola) in Bash consiste nel\
  \ processare e manipolare dati tabellari memorizzati in formato di testo semplice.\u2026"
title: Lavorare con i CSV
---

{{< edit_this_page >}}

## Che cosa & Perché?
Lavorare con file CSV (Valori Separati da Virgola) in Bash consiste nel processare e manipolare dati tabellari memorizzati in formato di testo semplice. Questo è fondamentale per i programmatori poiché consente l'automazione di compiti di trasformazione, analisi e integrazione dei dati direttamente dalla riga di comando, senza la necessità di strumenti più pesanti o ambienti di programmazione.

## Come fare:

**Leggere un File CSV Linea per Linea**

```bash
while IFS=, read -r colonna1 colonna2 colonna3
do
  echo "Colonna 1: $colonna1, Colonna 2: $colonna2, Colonna 3: $colonna3"
done < sample.csv
```

*Esempio di output:*

```
Colonna 1: id, Colonna 2: nome, Colonna 3: email
...
```

**Filtrare le Righe CSV Basandosi su una Condizione**

Utilizzando `awk`, puoi facilmente filtrare le righe. Per esempio, per trovare le righe dove la seconda colonna è uguale ad "Alice":

```bash
awk -F, '$2 == "Alice" { print $0 }' sample.csv
```

**Modificare il Valore di una Colonna**

Per cambiare la seconda colonna in maiuscolo:

```bash
awk -F, 'BEGIN {OFS=",";} { $2 = toupper($2); print $0; }' sample.csv
```

**Ordinare un File CSV Basandosi su una Colonna**

Puoi ordinare un file CSV basandosi, diciamo, sulla terza colonna (numericamente):

```bash
sort -t, -k3,3n sample.csv
```

**Usare `csvkit` per Compiti Più Complessi**

`csvkit` è un insieme di strumenti da linea di comando per convertire e lavorare con CSV. Può essere installato tramite pip.

Per convertire un file JSON in CSV:

```bash
in2csv data.json > data.csv
```

Per interrogare un file CSV usando SQL:

```bash
csvsql --query "SELECT name FROM sample WHERE id = 10" sample.csv
```

*Nota: Installare `csvkit` richiede Python e può essere fatto usando `pip install csvkit`.*
