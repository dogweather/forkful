---
title:    "Bash: Lettura degli argomenti della riga di comando"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché Leggere gli Argomenti della Riga di Comando

Leggere gli argomenti della riga di comando è un'abilità fondamentale per ogni programmatore Bash. Essi permettono di rendere il nostro codice più flessibile e permettono agli utenti di passare facilmente input al nostro script. Continua a leggere per scoprire come farlo!

## Come Leggere gli Argomenti della Riga di Comando

Per leggere gli argomenti della riga di comando in Bash, è necessario utilizzare la variabile speciale "$1" che rappresenta il primo argomento passato dopo il nome dello script. Ad esempio, supponiamo di avere uno script chiamato "hello.sh" a cui viene passato il nome di una persona come argomento:

```Bash
#!/bin/bash

echo "Ciao $1!"
```
Se eseguiamo questo script con il comando `./hello.sh Mario`, il risultato sarà `Ciao Mario!`. Possiamo anche passare più argomenti e accedervi utilizzando le variabili "$2", "$3", e così via.

Inoltre, possiamo utilizzare la variabile "$@" per accedere a tutti gli argomenti passati, invece di doverli specificare uno per uno.

## Approfondimenti su come Leggere gli Argomenti della Riga di Comando

Oltre all'uso delle variabili speciali, esistono anche alcune opzioni che possiamo utilizzare per gestire gli argomenti della riga di comando in modo più flessibile. Ad esempio, possiamo utilizzare l'opzione "-e" per accedere agli argomenti tramite indici anziché assegnarli a delle variabili.

Possiamo anche utilizzare l'opzione "-h" per mostrare un messaggio di aiuto agli utenti che passano argomenti errati o non passano alcun argomento.

Per maggiori informazioni su queste opzioni e altre, consulta la documentazione ufficiale di Bash.

## Vedi Anche

- Documentazione ufficiale di Bash: https://www.gnu.org/software/bash/manual/bash.html
- Tutorial su come scrivere script Bash: https://linuxconfig.org/bash-scripting-tutorial-for-beginners
- Esempi di script Bash per principianti: https://linuxconfig.org/bash-scripting-tutorial-for-beginners