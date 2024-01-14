---
title:                "Bash: Creazione di un file temporaneo"
simple_title:         "Creazione di un file temporaneo"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché

Creare un file temporaneo è un'operazione comune nel mondo della programmazione Bash. Questo tipo di file viene utilizzato per una varietà di scopi, come salvare dati temporanei o creare backup temporanei.

## Come fare

Per creare un file temporaneo in Bash, utilizzeremo il comando `mktemp`. Questo comando ci permette di creare un file con un nome univoco e di specificare la sua posizione.

Ecco un esempio di come creare un file temporaneo in Bash:

```Bash
#!/bin/bash

temp_file=$(mktemp) #crea un file temporaneo e salva il suo nome nella variabile $temp_file
echo "Questo è un file temporaneo" >> $temp_file #scrive una frase nel file temporaneo
cat $temp_file #stampa il contenuto del file temporaneo
```

Output:

```
Questo è un file temporaneo
```

In questo esempio, abbiamo utilizzato il comando `mktemp` per creare il file temporaneo e il comando `echo` per scrivere una frase all'interno. Infine, abbiamo utilizzato il comando `cat` per stampare il contenuto del file temporaneo. 

## Deep Dive

Il comando `mktemp` ci offre anche la possibilità di specificare un prefisso per il nome del file temporaneo e di scegliere in quale directory creare il file.

Ecco un esempio di come creare un file temporaneo con un prefisso personalizzato e in una specifica directory:

```Bash
#!/bin/bash

temp_file=$(mktemp -p /home/utente/Scrivania/ -t mytempfile) #crea un file temporaneo con prefisso mytempfile nella directory specificata
echo "Questo è un file temporaneo con prefisso personalizzato" >> $temp_file
cat $temp_file
```

Output:

```
Questo è un file temporaneo con prefisso personalizzato
```

È importante notare che, una volta che il programma è terminato, il file temporaneo verrà automaticamente rimosso dal sistema.

## Vedi anche

- [Comando mktemp nel manuale di Bash](https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html)
- [Articolo su come creare file temporanei in Bash](https://www.tecmint.com/create-temporary-files-in-bash/)
- [Guida per principianti alla programmazione in Bash](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)