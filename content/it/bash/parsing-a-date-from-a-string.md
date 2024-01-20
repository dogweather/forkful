---
title:                "Analizzare una data da una stringa"
html_title:           "Fish Shell: Analizzare una data da una stringa"
simple_title:         "Analizzare una data da una stringa"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Che Cos’è & Perché?

L'analisi di una data da una stringa è il processo di conversione di una data presentata come stringa in un formato di data leggibile dal programma. Questo è essenziale per manipolare e utilizzare correttamente le date nei tuoi script.

## Come fare:

Per convertire una stringa in una data in Bash, possiamo utilizzare il comando `date -d`. Ad esempio:

```bash
stringData="2021-09-23"
data=$(date -d "$stringData" +"%d/%m/%Y")
echo $data
```
Il codice restituirà:

```
23/09/2021
```

## Approfondimento

- **Contesto storico**: Bash è sempre stato utilizzato come linguaggio di scripting per la manipolazione di stringhe e file. La funzione `date -d` esiste fin dall'inizio ed è molto utile quando devi analizzare le stringhe delle date.

- **Alternative**: Se hai bisogno di una maggiore flessibilità nella formattazione della data, potresti considerare l'uso di `awk` o `perl`. Ad esempio, puoi facilmente spostare l'ordine del giorno e del mese con `awk`.

- **Dettagli di implementazione**: Il comando `date -d` accetta una stringa (come "2021-09-23") e cerca di interpretarla come data. Se riesce, formatta la data in base al formato fornito (come "%d/%m/%Y"). Se fallisce, restituisce un errore.

## Vedi Anche

- [Manuale Bash](https://www.gnu.org/software/bash/manual/bash.html#Shell-Commands): Documentazione ufficiale Bash.
- [Parsing delle date in Unix](https://stackoverflow.com/questions/6411509/how-to-parse-a-date-in-bash): Discussione di StackOverflow sul parsing delle date in Bash.