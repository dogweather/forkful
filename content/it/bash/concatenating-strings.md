---
title:                "Concatenazione di stringhe"
html_title:           "Bash: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?
Concatenare le stringhe è il processo di unire più stringhe in una singola stringa più lunga. I programmatori lo fanno per creare delle frasi o dei blocchi di testo che devono essere stampati o utilizzati all'interno del codice.

## Come fare:
Ecco alcuni esempi di come concatenare le stringhe in Bash:

```
stringa1="Benvenuto a "
stringa2="questo fantastico articolo!"
echo $stringa1$stringa2
```
Output: `Benvenuto a questo fantastico articolo!`

```
nome="Marco"
cognome="Rossi"
echo "Il mio nome completo è" $nome$cognome
```
Output: `Il mio nome completo è Marco Rossi`

```
lunghezza1=${#stringa1}
echo "La lunghezza della prima stringa è di $lunghezza1 caratteri."
```
Output: `La lunghezza della prima stringa è di 13 caratteri.`

## Approfondimento:
Per comprendere meglio il concetto di concatenazione delle stringhe, è utile conoscere la storia del linguaggio di programmazione Bash. Inoltre, esistono anche alternative per concatenare le stringhe, come ad esempio l'utilizzo del comando `printf`. Inoltre, l'implementazione della concatenazione delle stringhe in Bash è basata su un semplice meccanismo di sostituzione di variabili.

## Vedi anche:
- Documentazione ufficiale di Bash: https://www.gnu.org/software/bash
- Esempi di concatenazione delle stringhe in Bash: https://www.baeldung.com/linux/bash-string-concatenation
- Altri approfondimenti sul linguaggio Bash: https://linux.die.net/man/1/bash