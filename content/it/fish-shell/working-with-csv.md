---
title:                "Lavorare con i file CSV"
date:                  2024-01-19
html_title:           "Bash: Lavorare con i file CSV"
simple_title:         "Lavorare con i file CSV"

category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa e Perché?)
Lavorare con i CSV significa manipolare file di testo strutturati come "valori separati da virgola" (CSV); è essenziale per gestire dati tabellari. I programmatori lo fanno per importare, analizzare, e manipolare grandi volumi di dati in modo semplice ed efficace.

## How to: (Come fare:)
Ecco qualche comando base in Fish Shell per manipolare file CSV:

```Fish Shell
# Contare le righe di un file CSV
wc -l file.csv

# Stampare le prime 10 righe di un file CSV
head -n 10 file.csv

# Estrarre la prima colonna di un file CSV
awk -F"," '{print $1}' file.csv

# Ordinare un file CSV basandosi sulla seconda colonna
sort -t, -k2 file.csv

# Convertire un file CSV in formato tabella
column -s, -t < file.csv
```

Esempio di output:
```
10 file.csv
id,name,age
1,Marco,21
2,Giulia,25
...
```

## Deep Dive (Approfondimento)
Il formato CSV risale ai primi computer e si è affermato per la sua semplicità. È meno verboso di formati come XML o JSON e quindi più leggero e veloce da processare. Tuttavia, non ha uno standard rigoroso, quindi la struttura può variare. Strumenti alternativi includono librerie specifiche per il linguaggio di programmazione, come pandas in Python o CSV in Ruby. In Fish Shell, lavorare con CSV prevede principalmente l'uso di strumenti Unix standard come awk, sed, grep, sort e column.

## See Also (Vedi Anche)
- Manuale di Fish Shell: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- Tutorial AWK: [https://www.gnu.org/software/gawk/manual/gawk.html](https://www.gnu.org/software/gawk/manual/gawk.html)
- Guida agli strumenti di testo UNIX: [https://www.gnu.org/software/coreutils/manual/html_node/index.html](https://www.gnu.org/software/coreutils/manual/html_node/index.html)
