---
title:                "Generazione di numeri casuali"
html_title:           "Bash: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Cosa ed E perché?
Generare numeri casuali è una tecnica di programmazione che permette di ottenere un numero a caso all'interno di un intervallo definito. I programmatori spesso utilizzano questa tecnica quando hanno bisogno di una scelta casuale o di una variante nei loro programmi.

## Come fare:
Per generare un numero casuale in Bash, è possibile utilizzare il comando ```$RANDOM```. Questo comando restituisce un numero casuale compreso tra 0 e 32767 ogni volta che viene eseguito. È anche possibile specificare un intervallo personalizzato utilizzando la sintassi ```$(($RANDOM % intervallo + offset))```, dove "intervallo" indica l'intervallo desiderato e "offset" indica il numero da cui iniziare. Di seguito è riportato un esempio di codice e il relativo output:

```
# Genera un numero casuale compreso tra 1 e 10
echo $(( $RANDOM % 10 + 1 ))
# Output: 7

# Genera un numero casuale compreso tra 50 e 100
echo $(( $RANDOM % 51 + 50 ))
# Output: 73
```

## Approfondimento:
La generazione di numeri casuali è stata una tecnica ampiamente utilizzata nella programmazione sin dagli anni '60. In passato, i programmatori dovevano utilizzare formule matematiche complesse per ottenere numeri casuali, mentre oggi è diventato molto più semplice grazie ai comandi predefiniti come ```$RANDOM```. Alcune alternative alla generazione di numeri casuali in Bash sono l'utilizzo di strumenti esterni come il programma "shuf" o lo sviluppo di script Bash più sofisticati che utilizzano algoritmi più complessi per la generazione di numeri casuali.

## Vedi anche:
- Documentazione Bash su "RANDOM": https://www.gnu.org/software/bash/manual/html_node/Bash-Variables.html#index-RANDOM
- Esempi di utilizzo di "shuf": https://www.tecmint.com/shuf-command-examples/