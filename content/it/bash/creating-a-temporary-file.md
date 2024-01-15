---
title:                "Creazione di un file temporaneo"
html_title:           "Bash: Creazione di un file temporaneo"
simple_title:         "Creazione di un file temporaneo"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché

Creare un file temporaneo può essere utile quando hai bisogno di archiviare temporaneamente dei dati o creare un file di supporto per uno script Bash.

## Come fare

Ecco un esempio di codice che crea un file temporaneo chiamato "test.txt" e ci scrive dentro un messaggio di testo:

```Bash
# Crea un file temporaneo
tempfile=$(mktemp)

# Stampa il percorso del file temporaneo
echo $tempfile

# Scrivi un messaggio di testo nel file
echo "Questo è un file temporaneo creato con Bash" > $tempfile

# Verifica che il messaggio sia stato scritto correttamente
cat $tempfile
```

Output:
```
/tmp/tmp.YxXnKl
Questo è un file temporaneo creato con Bash
```

## Approfondimento

La funzione `mktemp` viene utilizzata per creare un file temporaneo in Bash. Questa funzione genererà un nome univoco per il file utilizzando una combinazione di lettere e numeri casuali e lo creerà nella directory temporanea del sistema, che di solito è `/tmp`.

Inoltre, è possibile specificare un modello per il nome del file temporaneo utilizzando il parametro `-p` seguito dal percorso della directory in cui si desidera creare il file temporaneo, ad esempio `-p /home/utente/Scrivania/`.

Una volta che il file temporaneo è stato creato, è possibile utilizzarlo come un normale file all'interno dello script, ad esempio scrivendo o leggendo dati al suo interno. Tuttavia, è importante ricordare di eliminare il file temporaneo una volta che non è più necessario, sia manualmente tramite il comando `rm`, o automaticamente utilizzando il comando `trap` per catturare l'interruzione del processo e cancellare il file.

## Vedi anche

- [Documentazione ufficiale di mktemp](https://www.gnu.org/software/coreutils/manual/html_node/mktemp-invocation.html)
- [Guida Bash - Creare file temporanei](https://linuxconfig.org/bash-scripting-tutorial-temporary-files)