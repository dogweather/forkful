---
title:                "Bash: Creazione di un file temporaneo"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché

Creare un file temporaneo può essere utile quando si lavora con script Bash per salvare informazioni temporanee o per manipolare dati anche in ambienti limitati.

## Come fare

Creare un file temporaneo in Bash è semplice, basta utilizzare il comando `mktemp`. Di seguito un esempio di codice e la corrispondente output:

```Bash
#!/bin/bash

# Creazione di un file temporaneo
tempfile=$(mktemp)

# Aggiunta di contenuti al file
echo "Ciao a tutti!" >> $tempfile
echo "Questo è un file temporaneo." >> $tempfile

# Stampa del contenuto del file
cat $tempfile

# Rimozione del file temporaneo
rm $tempfile
```

Output:

```
Ciao a tutti!
Questo è un file temporaneo.
```

Si noti che il comando `mktemp` crea un file con un nome unico e casuale, evitando così conflitti di nomi con file preesistenti.

## Approfondimento

Per creare un file temporaneo con un nome specifico è possibile utilizzare l'opzione `-p` seguita dal percorso desiderato. È anche possibile specificare un prefisso per il nome del file temporaneo utilizzando l'opzione `-t`. Inoltre, il comando `mktemp` può essere utile anche in combinazione con il comando `trap` per assicurarsi che il file venga rimosso anche in caso di interruzione dell'esecuzione dello script.

## Vedi Anche

- [Manuale di riferimento di Bash](https://devdocs.io/bash/)
- [Comandi di shell di base](https://www.geeksforgeeks.org/basic-shell-commands/#mktemp)
- [Tutorial di scripting Bash](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)