---
title:                "Creare un file temporaneo"
html_title:           "Arduino: Creare un file temporaneo"
simple_title:         "Creare un file temporaneo"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?
Creare un file temporaneo in Bash significa generare un file che esiste solo per il tempo necessario alla tua esecuzione del script. Gli sviluppatori lo fanno per gestire lo storage di dati transitori senza intasare il sistema.

## Come si fa:
Ecco un esempio di come potrebbe funzionare in Bash:
```Bash
#!/bin/bash

# Crea un file temporaneo
temp_file=$(mktemp)

echo "Questo è un test" >> $temp_file

# Stampa il contenuto del file temporaneo
cat $temp_file

# Elimina il file temporaneo
rm $temp_file
```

Ecco l'output dell'esempio:

```Bash
Questo è un test
```

## Deep Dive
La creazione di file temporanei in Bash ha una lunga storia. Al volo, il comando `mktemp` risale agli inizi di UNIX. Questo comando genera un nome di file unico, ma è compito del tuo script creare il file e gestirlo correttamente.

Alternative esistono: il comando `tempfile`, per esempio, fa praticamente la stessa cosa. Attenzione però, `tempfile` è deprecato in molti sistemi. Un altro modo è l'utilizzo diretto della libreria C, ma questo va oltre lo scopo di questo articolo.

Da notare che `mktemp` genera solo il nome del file temporaneo: il file stesso non viene creato fino a quando non si esegue un comando che lo fa (come `echo` nel nostro esempio). Inoltre, rimuovete sempre i file temporanei quando avete finito. Non sperate nel sistema operativo: potrebbe non farlo, o farlo troppo tardi.

## Vedi Anche
Ulteriori dettagli possono essere trovati nelle pagine man di Bash (`man bash`), mktemp (`man mktemp`) e tempfile (`man tempfile`), così come in numerosi tutorial e guide online. Se vuoi approfondire il scripting in Bash, controlla [questo link](https://www.gnu.org/software/bash/manual/bash.html) per la documentazione ufficiale di Bash.