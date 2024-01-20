---
title:                "Lavorare con i file CSV"
html_title:           "Bash: Lavorare con i file CSV"
simple_title:         "Lavorare con i file CSV"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Lavorare con CSV (valori separati da virgole) significa manipolare dati strutturati in un formato testuale semplice. I programmatori lo fanno perché il formato CSV è universale, leggibile e facile da esportare da database e fogli di calcolo.

## How to:

Ecco alcuni comandi Bash per gestire file CSV:

```Bash
# Contare il numero di righe
wc -l file.csv

# Stampare le prime 10 righe
head -n 10 file.csv

# Stampare solo la seconda colonna usando cut
cut -d',' -f2 file.csv

# Ordinare i dati basati sulla prima colonna
sort -t',' -k1,1 file.csv

# Cerca righe che contengano "esempio"
grep "esempio" file.csv
```

Esempio di output dopo l'esecuzione del comando `cut`:

```Bash
ValoreColonna2
AltroValore
AncorUnValore
```

## Deep Dive

Il formato CSV ha origini che risalgono agli anni '70 e, nonostante semplicità, è rimasto molto popolare. Alternativamente, oggi esistono formati come JSON o XML che supportano dati più complessi, ma la facilità d'uso del CSV è insuperabile per dataset semplici. Implementando comandi Unix come `cut`, `sort` e `grep`, possiamo manipolare dati CSV in modo efficiente direttamente dalla linea di comando, senza bisogno di software specifico.

## See Also

- Documentazione di GNU Coreutils: https://www.gnu.org/software/coreutils/
- Introduzione al formato CSV: https://it.wikipedia.org/wiki/Comma-separated_values
- Tutorial su come usar `awk` con i CSV: https://www.gnu.org/software/gawk/manual/gawk.html