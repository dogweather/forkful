---
title:                "Lettura di un file di testo."
html_title:           "Bash: Lettura di un file di testo."
simple_title:         "Lettura di un file di testo."
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Leggere un file di testo è una delle attività più comuni quando si lavora con la programmazione Bash. Tramite questa azione è possibile leggere e analizzare il contenuto di un file e utilizzarlo per eseguire ulteriori operazioni.

## Come Fare

Per leggere un file di testo con Bash, è necessario utilizzare il comando `cat`. Questo comando stampa tutto il contenuto di un file sullo standard output. Ecco un esempio di come utilizzare il comando `cat`:

```Bash
cat file.txt
```

Nell'esempio sopra, il comando `cat` legge il contenuto del file "file.txt" e lo stampa sullo standard output.

Se si vuole specificare una parte specifica del file da leggere, è possibile utilizzare il comando `head` o `tail`. Questi comandi mostrano rispettivamente le prime o le ultime righe di un file, e possono essere combinati con il comando `cat` per impostare il numero di righe da leggere. Ad esempio:

```Bash
head -n 10 file.txt
```

questo comando mostrerà le prime 10 righe del file "file.txt".

## Approfondimento

Ci sono diverse opzioni disponibili per leggere un file di testo con Bash. Una di queste è l'uso dei comandi `grep` e `sed`, che permettono di cercare e manipolare specifici pattern all'interno del file di testo.

È anche possibile eseguire operazioni più complesse con l'utilizzo delle espressioni regolari, che permettono di definire pattern più avanzati per la ricerca all'interno dei file.

## Vedi Anche

- Manuale Bash: https://www.gnu.org/software/bash/manual/

- Tutorial Bash: https://linuxconfig.org/bash-scripting-tutorial-for-beginners