---
title:                "Fish Shell: Eliminazione di caratteri corrispondenti ad un modello"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché
Quando si lavora con la Shell Fish, è possibile che ci si trovi a dover eliminare caratteri corrispondenti a un certo pattern. Questo potrebbe essere necessario per eseguire una correzione o per pulire i dati in una stringa.

## Come fare
Utilizzare il comando `string match` in Fish Shell per trovare e eliminare i caratteri corrispondenti al pattern desiderato. Ad esempio, se vogliamo eliminare tutti i numeri da una stringa, possiamo utilizzare il seguente codice:

```Fish Shell
set stringa “Questo è un esempio 123 di una stringa”
set pattern “[0-9]”
set nuova_stringa (string match -d -- $pattern $stringa)
```
Il comando `string match` ci permette di specificare il pattern che vogliamo cercare (`[0-9]` in questo caso) e poi utilizziamo l'opzione `-d` per indicare che vogliamo eliminare i caratteri corrispondenti. Infine, usiamo `--` per indicare che il pattern si applica solo alla stringa e non ai comandi successivi.

Il risultato sarà una nuova stringa senza i caratteri numerici: `Questo è un esempio di una stringa`.

## Approfondimento
Per effettuare una ricerca più precisa e complessa, possiamo utilizzare espressioni regolari all'interno del comando `string match`. Ad esempio, se vogliamo eliminare tutti i caratteri speciali da una stringa, possiamo utilizzare il seguente codice:

```Fish Shell
set stringa “Q/uest!o è un'e&mpi,.,o di una strin'ga”
set pattern “[^a-zA-Z0-9\s]”
set nuova_stringa (string match -d -- $pattern $stringa)
```
In questo caso, stiamo specificando un pattern che selezioni tutti i caratteri diversi da lettere, numeri e spazi (indicati con `[^a-zA-Z0-9\s]`). Il risultato sarà una nuova stringa pulita, senza caratteri speciali: `Q u est o è un e mpi o di una strin ga`.

## Vedi anche
- [Documentazione ufficiale di Fish Shell](https://fishshell.com/docs/current/index.html)
- [Guida alle espressioni regolari](https://www.regular-expressions.info/)
- [Guida introduttiva a Fish Shell](https://scotch.io/tutorials/getting-started-with-fish-the-friendly-interactive-shell)