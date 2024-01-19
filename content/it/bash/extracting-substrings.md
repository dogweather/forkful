---
title:                "Estrazione di sottosequenze"
html_title:           "Arduino: Estrazione di sottosequenze"
simple_title:         "Estrazione di sottosequenze"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?

L'estrazione delle sottostringhe è un'operazione che consente di ottenere una porzione (o "substring") di una stringa più grande. Questo è molto utile per i programmatori per manipolare i dati in base alle esigenze specifiche, come la ricerca e l'analisi del testo.

## Come fare:

Ecco un esempio di come estrarre una substring in Bash:
```Bash
stringa="Buongiorno mondo!"
echo ${stringa:10:5} 
```
E l'output sarebbe:
```Bash
mondo
```
In questo esempio, stiamo estraendo la sottostringa a partire dall'11° carattere (gli indici in shell scripting partono da 0) per 5 caratteri.

## Approfondimento

Nel contesto storico, Bash (nato nel 1989) ha aggiunto questa funzionalità molto dopo altri linguaggi di programmazione. Tuttavia, Bash è popolare per i suoi potenti strumenti di manipolazione delle stringhe, insieme ad altri vantaggi come la portabilità.

Parlando di alternative, ci sono molti altri linguaggi di programmazione che offrono modi diversi per estrarre sottostringhe - come Python, JavaScript e Java - ognuno con le proprie peculiarità.

Per quanto riguarda i dettagli di implementazione, l'operatore di estrazione delle sottostringhe in Bash è molto semplice. La sintassi è `${string:position:length}`, dove `string` è la stringa originale, `position` è la posizione iniziale da cui si desidera estrarre la sottostringa e `length` è il numero di caratteri da estrarre.

## Approfondimenti

Consultare i seguenti link per un approfondimento sull'argomento:

- Manipolazione delle stringhe in Bash: https://www.thegeekdiary.com/bash-how-to-do-string-manipulation/
- Bash substring: https://www.baeldung.com/linux/bash-substring
- Esempi di estrazione delle sottostringhe in altri linguaggi di programmazione: https://www.w3schools.com/python/ref_string_substring.asp
- Bash Scripting Guide: https://tldp.org/LDP/Bash-Beginners-Guide/html/index.html