---
title:                "Fish Shell: Scrittura su standard error"
simple_title:         "Scrittura su standard error"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivere su standard error è un processo importante per chiunque stia utilizzando il Fish Shell. Può aiutare a risolvere eventuali errori o semplicemente a visualizzare informazioni di debug durante l'esecuzione di un programma.

## Come fare

Per scrivere su standard error nel Fish Shell, è possibile utilizzare il comando "echoerr". Basta inserire il testo che si desidera visualizzare tra le virgolette e il messaggio verrà stampato su standard error.

```
Fish Shell

echoerr "Questo è un esempio di messaggio di errore"
```

Output:

Questo è un esempio di messaggio di errore

## Approfondimento

Scrivere su standard error può essere utile quando si desidera visualizzare informazioni di debug o messaggi di errore mentre si esegue un programma. Inoltre, è possibile utilizzare il comando "when" per specificare condizioni in cui il messaggio deve essere stampato su standard error.

Ad esempio, possiamo stampare un messaggio di errore solo quando una variabile è vuota:

```
Fish Shell

set -x my_variable

when not set my_variable
    echoerr "La variabile my_variable è vuota"
```

In questo modo, il messaggio verrà stampato solo quando la variabile non è stata inizializzata.

## Vedi anche

- [Documentazione Fish Shell](https://fishshell.com/docs/current/index.html)
- [Tutorial Fish Shell](https://fishshell.com/docs/current/tutorial.html)
- [Comandi Fish Shell](https://fishshell.com/docs/current/commands.html)