---
title:                "Convertire una stringa in minuscolo"
html_title:           "Arduino: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?

Convertire una stringa in minuscolo significa rendere ogni carattere alfabetico in essa una versione minuscola del se stesso. Programmare per fare ciò è importante nell'interazione con utenti ed dati, dato che garantisce consistenza e previene errori dovuti a differenze di maiuscolo/minuscolo.

## Come fare:

Ecco come si fa con Fish Shell:

```Fish Shell
set var 'CIAO MONDO'
set var (string lower $var)
echo $var
```

Output:

```Fish Shell
ciao mondo
```

La stringa 'CIAO MONDO' viene convertita in 'ciao mondo'.

## Approfondimento:

Converting stringhe in minuscolo è una pratiche antiche nelle scienze informatiche. Nei primi giorni i computer erano case sensitive, e therefore 'Hello' e 'hello' sarebbero stati distinti come due parole diverse.

Un'alternativa potrebbe coinvolgere l'uso di strumenti come 'awk' o 'tr', ma Fish Shell offre una funzione di built-in 'string lower' che è molto più pulita ed efficiente.

In termini di implementazione, 'string lower' attraversa ogni carattere della stringa, controllando se è un carattere maiuscolo. Se lo è, lo converte in minuscolo utilizzando la relazione ASCII tra lettere maiuscole e minuscole.

## Guarda Anche:

Per approfondimenti sulla conversione di stringhe e sulla programmazione in Fish Shell, controllare i seguenti link:

1. Documentazione ufficiale per Fish Shell: https://fishshell.com/docs/current/index.html
2. Guida completa alle stringhe in Fish Shell: https://fishshell.com/docs/current/cmds/string.html
3. Un articolo utile che tratta della conversione in minuscolo: https://www.thegeekstuff.com/2010/07/bash-cases-statements/