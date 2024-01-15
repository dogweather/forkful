---
title:                "Ricerca e sostituzione di testo"
html_title:           "Bash: Ricerca e sostituzione di testo"
simple_title:         "Ricerca e sostituzione di testo"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché
La ricerca e la sostituzione di testo sono operazioni comuni nel mondo della programmazione e della gestione di file di testo. Consentono di risparmiare tempo ed evitare errori nel modificare grandi quantità di testo.

## Come fare
La Bash è uno strumento potente per la gestione di file di testo, grazie alla presenza di comandi e operatori specifici per la ricerca e la sostituzione di testo. Ecco alcuni esempi pratici:

### Ricerca e sostituzione su un singolo file
Per cercare e sostituire un determinato testo in un file chiamato "file.txt", utilizziamo il comando `sed` seguito dai parametri `-i` (per modificare direttamente il file) e `-e` (per specificare il comando di ricerca e sostituzione). Ad esempio, se volessimo sostituire tutte le occorrenze di "cane" con "gatto" all'interno del file, utilizziamo il seguente comando:

```Bash
sed -i -e 's/cane/gatto/g' file.txt
```

### Ricerca e sostituzione su più file
Se volessimo effettuare la stessa operazione su una serie di file, possiamo utilizzare il comando `sed` seguito dai parametri `-i` e `-e`, ma questa volta aggiungendo anche il carattere `*` per indicare tutti i file presenti nella cartella corrente. Ad esempio:

```Bash
sed -i -e 's/cane/gatto/g' *
```

### Utilizzo di espressioni regolari
La Bash permette di utilizzare espressioni regolari per effettuare ricerche e sostituzioni ancora più precise. Ad esempio, se volessimo sostituire tutte le parole di tre lettere con "xyz" all'interno di un file, possiamo utilizzare il seguente comando:

```Bash
sed -i -e 's/\b[a-z]\{3\}\b/xyz/g' file.txt
```

## Approfondimento
La Bash utilizza il comando `sed` (stream editor) per effettuare ricerche e sostituzioni su file di testo. Questo comando si basa sulla sintassi di espressioni regolari e offre numerose opzioni per gestire e manipolare il testo. Per saperne di più su come utilizzare `sed`, è possibile consultare la sua pagina man digitando `man sed` nel terminale.

## Vedi anche
- Manuale di `sed`: https://www.gnu.org/software/sed/manual/sed.html
- Guida all'utilizzo delle espressioni regolari: https://www.digitalocean.com/community/tutorials/the-basics-of-using-the-sed-stream-editor-to-manipulate-text-in-linux