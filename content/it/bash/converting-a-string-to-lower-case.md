---
title:                "Bash: Convertire una stringa in minuscolo"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui potresti voler convertire una stringa in lettere minuscole nel tuo programma Bash. Forse stai manipolando dei dati da un'altra fonte e vuoi uniformarli per eseguire ulteriori operazioni su di essi. O forse vuoi semplicemente formattare una stringa in modo più leggibile per l'utente finale. Qualunque sia il motivo, imparare come convertire una stringa in lettere minuscole ti sarà sicuramente utile nelle tue attività di programmazione.

## Come fare

Per convertire una stringa in lettere minuscole in Bash, puoi utilizzare il comando `tr` (che sta per "translate" o "traslitterare"). Questo comando può essere utilizzato per sostituire un carattere con un altro all'interno di una stringa. Utilizzando `tr` insieme al comando `echo` e alle parentesi graffe `{}` per racchiudere la stringa, puoi facilmente ottenere una nuova stringa in lettere minuscole.

Un esempio di codice potrebbe essere il seguente:

```
Bash
echo "HELLO WORLD" | tr '[A-Z]' '[a-z]'
```

In questo esempio, abbiamo utilizzato `tr` per sostituire tutte le lettere maiuscole con le corrispondenti lettere minuscole all'interno della stringa "HELLO WORLD". Il risultato sarà:

```
hello world
```

Puoi anche utilizzare questo metodo per manipolare variabili di stringa all'interno di uno script Bash, come nel seguente esempio:

```
Bash
# dichiara una variabile di stringa
STRING="CONVERTI QUESTA STRINGA IN LETTERE MINUSCOLE"

# utilizza il comando `tr` per eseguire la conversione
LOWER_STRING=$(echo ${STRING} | tr '[A-Z]' '[a-z]')

# stampa il risultato
echo ${LOWER_STRING}
```

Il risultato sarà:

```
converti questa stringa in lettere minuscole
```

## Approfondimento

Oltre al comando `tr`, esistono altri modi per convertire una stringa in lettere minuscole in Bash. Puoi utilizzare il comando `sed` (stream editor) o il comando `awk` (linguaggio di programmazione orientato alle stringhe) per ottenere lo stesso risultato. Inoltre, ci sono alcune opzioni di `tr` che possono essere utili per adattare il comando alle tue esigenze.

Ad esempio, se vuoi convertire solo alcune lettere specifiche all'interno della stringa, puoi specificarle come argomenti per `tr` invece di utilizzare gli intervalli `[A-Z]` e `[a-z]`. Oppure, puoi utilizzare l'opzione `-d` per eliminare specifici caratteri (come lettere maiuscole) dalla stringa, invece di sostituirli con lettere minuscole.

## Vedi anche

* [Manuale di Bash](https://www.gnu.org/software/bash/manual/)
* [Documentazione su `tr`](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)
* [Documentazione su `sed`](https://www.gnu.org/software/sed/manual/sed.html)
* [Documentazione su `awk`](https://www.gnu.org/software/gawk/manual/gawk.html)