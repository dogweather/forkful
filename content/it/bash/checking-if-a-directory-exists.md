---
title:                "Verifica dell'esistenza di una cartella"
html_title:           "Bash: Verifica dell'esistenza di una cartella"
simple_title:         "Verifica dell'esistenza di una cartella"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Cos'è e perché lo si fa?

Controllare se una directory esiste è un'attività comune tra i programmatori. Consiste nel verificare se una determinata directory è presente nel sistema e nel comunicare il risultato al programma.

Perché lo facciamo? Semplicemente perché spesso i nostri programmi devono manipolare file o risorse presenti in determinate directory e, prima di farlo, è importante assicurarsi che queste directory esistano effettivamente.

## Come fare:

In Bash, esistono diversi modi di controllare l'esistenza di una directory. Uno dei più comuni è l'utilizzo dell'operatore `-d` insieme a `if`. Ad esempio:

```Bash
if [ -d "directory" ]; then
    echo "La directory esiste!"
else
    echo "La directory non esiste."
fi
```

Questo codice verifica se la directory "directory" esiste e stampa un messaggio appropriato in base al risultato.

Un altro modo di verificare l'esistenza di una directory è utilizzare il comando `test`, in particolare il flag `-d`. Ad esempio:

```Bash
if test -d "directory"; then
    echo "La directory esiste!"
else
    echo "La directory non esiste."
fi
```

Anche in questo caso, il codice verifica se la directory "directory" esiste e stampa un messaggio di conseguenza.

## Approfondimenti:

Esistono anche altre opzioni per controllare se una directory esiste in Bash, come ad esempio l'uso del comando `mkdir`, che restituirà un errore se la directory esiste già.

Inoltre, esistono anche modi di controllare l'esistenza di una directory in altri linguaggi di programmazione, come ad esempio Python o Java.

Per quanto riguarda l'implementazione, il controllo dell'esistenza di una directory è solitamente fatto utilizzando funzioni del sistema operativo, che forniscono informazioni sulle risorse presenti nel sistema.

## Vedi anche:

Se vuoi approfondire l'argomento, puoi consultare questi link:

- [BashGuide - Test Command](https://linux.die.net/Bash-Beginners-Guide/sect_07_01.html)
- [Utilizzo delle funzioni del sistema operativo in Bash](https://www.tldp.org/LDP/abs/html/intandnonint.html#PATHSUBST)
- [Controllare l'esistenza di una directory in Python](https://www.geeksforgeeks.org/python-check-if-a-directory-exists-and-create-if-it-doesnt/)