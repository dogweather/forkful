---
title:                "Estrazione di sottostringhe"
html_title:           "Bash: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Estrarre delle sottostringhe è un'operazione comune nella programmazione Bash, che consiste nel prelevare una parte di una stringa più grande. I programmatori spesso fanno uso di questa tecnica per ottenere informazioni specifiche da una stringa più ampia o per manipolare i dati in un formato particolare.

## Come:
Estrarre delle sottostringhe in Bash è possibile grazie al comando `substring`. Di seguito sono riportati alcuni esempi di codice e relativi output per mostrare come utilizzare questo comando:

```
# Estrarre i primi 5 caratteri di una stringa
STRINGA="Ciao Mondo!"
substr="${STRINGA:0:5}"
echo $substr

# Output: Ciao

# Estrarre l'estensione di un file
FILE="documenti/report.pdf"
substr="${FILE##*.}"
echo $substr

# Output: pdf
```

## Approfondimento:
L'operazione di estrarre delle sottostringhe non è un concetto nuovo, infatti risale all'epoca dei primi linguaggi di programmazione. Tuttavia, ci sono alternative al comando `substring` in Bash, come ad esempio l'utilizzo delle espressioni regolari. 

L'implementazione del comando `substring` in Bash è semplice e diretta, ma può causare confusione se non si è a conoscenza della sintassi corretta. È importante ricordare che la posizione dei caratteri all'interno di una stringa inizia da 0.

## Guarda Anche:
Per ulteriori informazioni sull'estrazione di sottostringhe in Bash, si consiglia di leggere la documentazione ufficiale [qui](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Shell-Parameter-Expansion). Inoltre, è possibile trovare utili esempi di codice su siti come Stack Overflow o GitHub.