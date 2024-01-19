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

## Che Cosa e Perché?
La concatenazione delle stringhe è il processo per unire due o più stringhe in un'unica. I programmatori lo fanno per creare nuove stringhe da sorgenti esistenti, risparmiando tempo e codice.

## Come Fare:

La concatenazione delle stringhe in Bash è piuttosto semplice, ecco alcuni esempi:

```Bash
#!/bin/bash
# possibilità 1:
stringa1="Buongiorno, "
stringa2="Mondo!"
echo $stringa1$stringa2
```

L'output sarà:
```Bash
Buongiorno, Mondo!
```

O un altro metodo:
```Bash
#!/bin/bash
# possibilità 2:
stringa1="Buongiorno, "
stringa1+="Mondo!"
echo $stringa1
```
L'output sarà lo stesso dell'esempio precedente.

## Approfondimento:
La concatenazione delle stringhe è un'operazione base visto fin dagli inizi del programmazione. In Bash, la concatenazione delle stringhe è una procedura semplice e diretta, ma ci sono altre alternative, come l'uso del comando 'printf'. Ad esempio, 

```Bash
#!/bin/bash
stringa1="Buongiorno, "
stringa2="Mondo!"
printf "%s%s\n" $stringa1 $stringa2
```

Relativamente all'implementazione, quando eseguite l'operazione di concatenazione, Bash in realtà non copia i caratteri individuali delle stringhe. Piuttosto, "collega" semplicemente le stringhe originari, rendendo l'operazione estremamente efficiente.

## Vedi Anche:

1. "Learn Bash in Y Minutes" (https://learnxinyminutes.com/docs/bash/): Una rapida panoramic sulla programmazione Bash.
2. "Bash String Manipulation Guide" (https://www.tldp.org/LDP/abs/html/string-manipulation.html): Una guida dettagliata sulle operazioni sulle stringhe in Bash.
3. "Advanced Bash-Scripting Guide" (http://www.tldp.org/LDP/abs/html/): Un'approfondita guida alla scriptatura Bash.