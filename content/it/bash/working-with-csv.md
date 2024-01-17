---
title:                "Lavorare con i file csv"
html_title:           "Bash: Lavorare con i file csv"
simple_title:         "Lavorare con i file csv"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Lavorare con CSV (Comma Separated Values) significa manipolare e gestire fogli di calcolo e tabelle in un formato compatibile con le applicazioni di database e software di analisi dei dati. I programmatori spesso lavorano con CSV per importare e esportare dati da e verso diverse applicazioni, come ad esempio database e fogli di calcolo.

## Come fare:

Ecco alcuni esempi di codice e relativi output utilizzando Bash per lavorare con CSV:

```Bash
# Esempio di creazione di un nuovo file CSV
echo "nome, cognome, età" > nuova_tabella.csv
echo "Giulia, Rossi, 25" >> nuova_tabella.csv
echo "Marco, Bianchi, 30" >> nuova_tabella.csv
echo "Chiara, Neri, 28" >> nuova_tabella.csv

# Esempio di lettura di un file CSV
while IFS=, read -r nome cognome età; do
  echo "Nome: $nome, Cognome: $cognome, Età: $età"
done < nuova_tabella.csv
```

Output:

```
Nome: Giulia, Cognome: Rossi, Età: 25
Nome: Marco, Cognome: Bianchi, Età: 30
Nome: Chiara, Cognome: Neri, Età: 28
```

## Approfondimento:

In origine, CSV fu introdotto come un formato standard per scambiare dati tra applicazioni di database e fogli di calcolo. Tuttavia, ora è diventato un formato comune per la gestione dei dati in molti altri contesti, come ad esempio l'importazione di dati in siti web o la creazione di report. Ci sono molte alternative per lavorare con CSV, tra cui l'utilizzo di linguaggi di scripting come Python o l'uso di librerie specifiche per CSV come CSVKit. Inoltre, è possibile personalizzare il comportamento del formato CSV utilizzando i separatori di campo e di riga più adatti alle proprie esigenze.

## Vedi anche:

- [Bash Reference Manual](https://www.gnu.org/software/bash/manual)
- [CSV su Wikipedia](https://en.wikipedia.org/wiki/Comma-separated_values)
- [CSVKit](https://csvkit.readthedocs.io/en/latest/)