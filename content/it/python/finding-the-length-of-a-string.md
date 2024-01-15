---
title:                "Trovare la lunghezza di una stringa"
html_title:           "Python: Trovare la lunghezza di una stringa"
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Trovare la lunghezza di una stringa è un'operazione fondamentale nella programmazione, poiché spesso è necessario manipolare e analizzare dati testuali nei nostri programmi. Conoscere il numero di caratteri presenti in una stringa può aiutarci a svolgere molte attività, come ad esempio controllare se una parola è presente o cercare una sottostringa all'interno di un testo.

## Come fare

Per trovare la lunghezza di una stringa in Python, possiamo utilizzare il metodo predefinito `len()`. Questo metodo accetta una stringa come argomento e restituisce il numero di caratteri presenti al suo interno. Ecco un esempio di codice e il relativo output:

```Python
stringa = 'ciao mondo'
lunghezza = len(stringa)
print(lunghezza)
```
Output: 10

In questo caso, la variabile `lunghezza` conterrà il valore 10, poiché la stringa "ciao mondo" contiene 10 caratteri.

Possiamo anche utilizzare il metodo `len()` direttamente all'interno di una espressione, come ad esempio in un ciclo `for` per scorrere una stringa:

```Python
for i in range(len(stringa)):
  print(stringa[i])
```
Output:
c
i
a
o
m
o
n
d
o

## Approfondimento

È interessante notare che il metodo `len()` funziona non solo su stringhe, ma anche su altri tipi di dati come liste, tuple e dizionari. Inoltre, è utile sapere che questo metodo è predefinito in Python e l'implementazione può variare a seconda del tipo di dato su cui viene utilizzato. Ad esempio, per le stringhe, il metodo conta il numero di caratteri, mentre per le liste conta il numero di elementi presenti al suo interno.

Un altro aspetto importante è che il metodo `len()` funziona anche con stringhe Unicode, quelle che contengono caratteri speciali come accenti o simboli stranieri. In questo caso, la lunghezza sarà calcolata considerando anche questi caratteri speciali, quindi può essere diversa dalla lunghezza calcolata con il metodo `len()` in altri linguaggi di programmazione.

## Vedi anche

- La documentazione ufficiale di Python sul metodo `len()`: https://docs.python.org/3/library/functions.html#len
- Un tutorial su come manipolare stringhe in Python: https://www.w3schools.com/python/python_strings.asp