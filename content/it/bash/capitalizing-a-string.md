---
title:                "Capitalizzare una stringa"
html_title:           "Bash: Capitalizzare una stringa"
simple_title:         "Capitalizzare una stringa"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Che cosa & Perche'?

Capitalizzare una stringa significa convertire tutte le lettere minuscole in lettere maiuscole. I programmatori lo fanno spesso per uniformare il testo oppure per confrontare due stringhe indipendentemente dalla loro capitalizzazione. 

## Come fare:

Bash offre una comoda funzione integrata, `tr`, per capitalizzare una stringa. Ecco un esempio di come farlo:

```Bash
testo="questa e' una stringa da capitalizzare"
echo $testo | tr [a-z] [A-Z]
```
*Output:* `QUESTA E' UNA STRINGA DA CAPITALIZZARE`

Un altro modo è utilizzare `sed` per sostituire i caratteri con la loro versione maiuscola:

```Bash
testo="questa e' una stringa da capitalizzare"
echo $testo | sed 's/.*/\U&/'
```

*Output:* `QUESTA E' UNA STRINGA DA CAPITALIZZARE`

## Approfondimento:

La capitalizzazione delle stringhe è stata introdotta nel linguaggio di programmazione AWK negli anni '70. È diventata una funzione standard negli anni successivi ed è supportata in molti linguaggi di programmazione, come Python e Java.

Ci sono anche altri metodi per capitalizzare una stringa in Bash, come utilizzare il built-in `bash` o un'azione `awk` più elaborata.

Per quanto riguarda le implementazioni, la funzione `tr` è più veloce di `sed`, ma `sed` è più flessibile e può essere utilizzato per capitalizzare solo alcune lettere all'interno di una stringa.

## Vedi anche:

- [Bash Reference Manual](https://www.gnu.org/software/bash/manual/html_node/Bash-Reference-Manual.html)
- [AWK User's Guide](https://www.gnu.org/software/gawk/manual/html_node/Capitalization.html)