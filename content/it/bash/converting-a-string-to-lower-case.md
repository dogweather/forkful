---
title:                "Bash: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché
Con convertire una stringa in minuscolo si può rendere il codice più leggibile e facilitare la comparazione tra stringhe. Inoltre, alcune funzioni richiedono input in minuscolo, quindi è importante saper convertire correttamente le stringhe.

## Come
Per convertire una stringa in minuscolo in Bash, è possibile utilizzare il comando `tr` insieme all'opzione `-s` per rimuovere eventuali doppioni di spazi e l'opzione `-T` per specificare che l'input deve essere considerato come una stringa. Ecco un esempio di codice:

```Bash
stringa="STRINGA IN MAIUSCOLO"
stringa_minuscola=$(echo "$stringa" | tr -s -T[:upper:] [:lower:])
echo $stringa_minuscola
```

L'output sarà `stringa in maiuscolo`, ovvero la stessa stringa ma convertita in minuscolo.

## Approfondimento
Ci sono alcuni punti importanti da tenere a mente quando si vuole convertire una stringa in minuscolo in Bash. Prima di tutto, il comando `tr` è case-sensitive, il che significa che se si vuole mantenere le lettere in maiuscolo, è necessario specificarlo nell'opzione `[:lower:]`, altrimenti tutte le lettere verranno convertite in minuscolo.

Inoltre, il comando `tr` funziona solo con lettere ASCII, quindi se la stringa contiene caratteri speciali o lettere accentate, potrebbe essere necessario utilizzare un altro approccio per la conversione.

Infine, è possibile utilizzare altre funzioni di Bash, come ad esempio `sed`, per convertire una stringa in minuscolo. Ad esempio:

```Bash
stringa="STRINGA IN MAIUSCOLO"
stringa_minuscola=$(echo $stringa | sed 's/.*/\L&/')
echo $stringa_minuscola
```

L'output sarà lo stesso della precedente soluzione con il comando `tr`.

## Vedi anche
- [Comando tr in manuale Linux](https://linux.die.net/man/1/tr)
- [Bash scripting tutorial](https://ryanstutorials.net/bash-scripting-tutorial/)
- [Guida introduttiva a sed](http://www.grymoire.com/Unix/Sed.html)