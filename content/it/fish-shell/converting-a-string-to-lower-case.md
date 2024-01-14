---
title:    "Fish Shell: Convertire una stringa in minuscolo"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Spesso nella programmazione dobbiamo manipolare delle stringhe di testo. Una delle operazioni più comuni è la conversione di una stringa in caratteri minuscoli. Questo può essere utile per confrontare le stringhe in modo case-insensitive o per uniformare i dati di input.

## Come fare

Per convertire una stringa in minuscolo utilizzando il Fish Shell, possiamo utilizzare il comando `string tolower` seguito dalla stringa che vogliamo convertire. Ad esempio:

```Fish Shell
set testString "Ciao a tutti!"
echo (string tolower $testString)
```

Questo producirà l'output `ciao a tutti!`.

Inoltre, se stai lavorando con variabili di ambiente, puoi utilizzare il comando `set` per assegnare il risultato della conversione a una nuova variabile. Ad esempio:

```Fish Shell
set originalString "FISH SHELL"
set lowercaseString (string tolower $originalString)
echo $lowercaseString # output: fish shell
```

## Approfondimento

Un fatto interessante è che la conversione di una stringa in minuscolo utilizzando il Fish Shell è effettuata in base al parametro di locale corrente. Questo significa che, a seconda del sistema in cui stai eseguendo il codice, il risultato potrebbe essere diverso.

Inoltre, esiste un altro modo per convertire una stringa in minuscolo utilizzando il Fish Shell, ovvero utilizzando il correttore ortografico integrato. Questo può essere fatto utilizzando il comando `correct` seguito dalla stringa da convertire. Ad esempio:

```Fish Shell
echo (correct "Hello World") # output: hello world
```

Questo è possibile poiché il correttore ortografico mostra le parole con la prima lettera maiuscola e il resto minuscolo.

## Vedi anche

- [Guida rapida sulla manipolazione delle stringhe utilizzando il Fish Shell](https://fishshell.com/docs/current/cmds/set.html#syntax)
- [Documentazione ufficiale del Fish Shell](https://fishshell.com/docs/current/index.html)
- [Tutorial sull'uso del Fish Shell per la programmazione](https://fishshell.com/docs/current/tutorial.html)