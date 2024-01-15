---
title:                "Lettura degli argomenti della riga di comando"
html_title:           "Bash: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore principiante o esperto, capire come funzionano gli argomenti della riga di comando può sembrare intimidatorio. Tuttavia, una volta che avrai imparato le basi, sarai in grado di scrivere script più potenti e automatizzare il tuo lavoro sul terminale. In questo articolo, imparerai come leggere gli argomenti della riga di comando utilizzando il linguaggio di scripting Bash, in modo semplice e conciso.

## Come fare

Per leggere gli argomenti della riga di comando in Bash, devi prima accedere a una variabile interna chiamata "$@", che contiene tutti gli argomenti passati al tuo script. Utilizzando un ciclo "for", puoi iterare su ogni argomento e manipolarlo come desideri.

```Bash
for arg in "$@"
do
  # codice per manipolare gli argomenti qui
done
```

Ad esempio, se vuoi stampare tutti gli argomenti passati dall'utente, puoi utilizzare il comando "echo" all'interno del ciclo "for", come mostrato di seguito:

```Bash
for arg in "$@"
do
  echo $arg
done
```

Questo codice stamperà ogni argomento su una riga separata quando lo esegui sul terminale. Se vuoi manipolare gli argomenti in modo più specifico, puoi utilizzare l'indice "[$@]" per accedere a un argomento specifico. Ad esempio, se vuoi manipolare solo il primo argomento, puoi utilizzare "$1", il secondo con "$2" e così via.

## Approfondimento

Oltre al simbolo "@", ci sono altre variabili interne che possono aiutarti a leggere e manipolare gli argomenti della riga di comando. Ad esempio, "$#" contiene il numero totale di argomenti passati, "$0" rappresenta il nome del tuo script e "$*" contiene tutti gli argomenti come una singola stringa invece di un elenco separato. Inoltre, puoi utilizzare il comando "getopts" per leggere gli argomenti con opzioni in modo più strutturato.

## Vedi anche

- [Documentazione ufficiale di Bash](https://www.gnu.org/software/bash/)
- [Tutorial di programmazione Bash su YouTube](https://www.youtube.com/watch?v=oxuRxtrO2Ag)
- [Esempi di script Bash su GitHub](https://github.com/learnbyexample/Command-line-text-processing/blob/master/basics/bash_example_scripts.sh)