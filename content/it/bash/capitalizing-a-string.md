---
title:                "Bash: Capitalizzare una stringa"
simple_title:         "Capitalizzare una stringa"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché Capitalizzare una Stringa in Bash

Capitalizzare una stringa in Bash è un'operazione comune quando si lavora con dati. Può essere utile per uniformare il formato di dati o per rendere più leggibili certi output.

## Come Fare

Per capitalizzare una stringa in Bash, possiamo utilizzare il comando `tr` insieme al parametro `-s`, che elimina i caratteri duplicati, e al parametro `-c` per specificare i caratteri da ignorare.

Ecco un esempio di codice che capitalizza la stringa "casa" in "CASA":

```Bash
echo "cAsA" | tr -sc '[:lower:]' '[:upper:]'
```

L'output sarà: 

```Bash
CASA
```


## Deep Dive

In Bash, le stringhe sono trattate come array di caratteri. Per capitalizzare una stringa, possiamo quindi ciclare attraverso ogni carattere e sostituire quelli minuscoli con quelli maiuscoli utilizzando il comando `tr`.

Un'altra opzione è utilizzare la variabile Bash `$BASH_REMATCH` in combinazione con il comando `[[...]]` per estrarre la stringa e capitalizzarla.

Inoltre, quando si lavora con testi multilingua, è importante tenere in considerazione le diverse regole di capitalizzazione nelle varie lingue.

## Vedi Anche

- [Guida per Principianti alla Programmazione Bash](https://www.makeuseof.com/tag/bash-programming-beginners-guide/)
- [Documentazione Ufficiale di Bash](https://www.gnu.org/software/bash/manual/html_node/)
- [Trasformare Stringhe in Bash](https://wiki.bash-hackers.org/syntax/pe#changing_case)