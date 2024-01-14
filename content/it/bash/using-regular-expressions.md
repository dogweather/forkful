---
title:                "Bash: Utilizzare le espressioni regolari"
simple_title:         "Utilizzare le espressioni regolari"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Perché

Le espressioni regolari sono utilizzate per trovare e manipolare testo in modo rapido ed efficiente. Sono uno strumento potente per controllare gli input dell'utente, estrarre informazioni da un testo e controllare la validità dei dati prima di elaborarli nel tuo codice Bash.

## Come utilizzarle

Per utilizzare le espressioni regolari in Bash, è necessario utilizzare il comando "grep". Ad esempio, per cercare una determinata parola in un file di testo, è possibile utilizzare il seguente codice:

```Bash
grep "parola" file.txt
```

Questo esempio restituirebbe tutte le righe del file che contengono la parola cercata. È possibile anche utilizzare espressioni regolari per cercare un determinato modello di testo. Ad esempio, per trovare tutte le parole che iniziano con la lettera "a", il codice sarebbe:

```Bash
grep "^a" file.txt
```

## Approfondimento

Le espressioni regolari possono diventare molto complesse e potenti. Ad esempio, è possibile utilizzare i simboli "*" e "?" per indicare corrispondenze multiple o facoltative, rispettivamente. Inoltre, le parentesi possono essere utilizzate per creare gruppi di espressioni all'interno di una regola.

Un'altra caratteristica utile delle espressioni regolari in Bash è che supportano anche l'utilizzo di "backreferences". Questi consentono di fare riferimento a gruppi precendenti all'interno della stessa espressione regolare. Ad esempio, è possibile utilizzare il codice seguente per trovare le parole duplicate in un testo:

```Bash
grep -E '([a-zA-Z]+).*\1' file.txt
```

Questo esempio troverà tutte le parole composte da almeno due lettere e ripetute una o più volte all'interno dello stesso file.

## Vedi anche

- [The Linux Command Line: A Complete Guide to the Linux Operating System](https://www.amazon.it/Linux-CommandLine-Complete-Bill-Shotts/dp/1593279523)
- [Introduzione alle espressioni regolari di Bash](https://www.linux.com/tutorials/introduction-regular-expressions/)
- [Bash Guide per principianti: Espressioni regolari](http://tldp.org/LDP/Bash-Beginners-Guide/html/sect_04_01.html)