---
title:                "Bash: Verifica dell'esistenza di una directory"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui potresti voler verificare se una directory esiste durante la scrittura di uno script Bash. Ad esempio, potresti dover controllare se una determinata directory è presente prima di creare un nuovo file al suo interno. O forse vuoi eseguire un'azione solo se una determinata directory esiste. In ogni caso, è sempre una buona pratica verificare la presenza di una directory prima di eseguire qualsiasi operazione su di essa.

## Come fare

Per verificare se una directory esiste in uno script Bash, puoi utilizzare il comando `test` o le parentesi tonde. Ad esempio:

```Bash
if [ -d "/percorso/directory" ]; then
  echo "La directory esiste!"
else
  echo "La directory non esiste."
fi
```

Nell'esempio sopra, stiamo utilizzando il comando `test` all'interno di una struttura `if-else` per verificare la presenza di una directory specifica. Se la directory esiste, verrà stampato un messaggio di conferma. Altrimenti, verrà stampato un messaggio di errore.

Another option is to use the `test` command with the `-e` flag, which checks if a file exists regardless of its type. For example:

```Bash
if [ -e "/path/file.txt" ]; then
  echo "Il file esiste!"
else
  echo "Il file non esiste."
fi
```

In questo caso, stiamo controllando se il file esiste indipendentemente dal fatto che sia una directory o un altro tipo di file. Puoi anche usare questo metodo per verificare la presenza di una directory.

## Approfondimento

Oltre ai comandi `test` e le parentesi tonde, esistono anche altre opzioni per verificare se una directory esiste in uno script Bash. Ad esempio, puoi utilizzare il comando `ls` combinato con il flag `-d` per ottenere un elenco delle directory all'interno di una determinata cartella. Se l'elenco non è vuoto, significa che la directory esiste.

Puoi anche utilizzare un ciclo `for` per controllare ogni elemento all'interno di una cartella e verificare se è una directory. Se una delle iterazioni del ciclo è una directory, allora significa che la directory esiste.

Inoltre, puoi impostare una variabile all'interno dello script che memorizza il percorso della directory e utilizzarla nelle operazioni successive per evitare di dover verificare la sua esistenza più volte.

## Vedi anche

- [Linuxize - How to Check if File or Directory Exists in Bash](https://linuxize.com/post/bash-check-if-file-exists/)
- [GeeksforGeeks - Checking if Directory Exists in Bash Shell Script](https://www.geeksforgeeks.org/checking-directory-exists-bash-shell-script/)