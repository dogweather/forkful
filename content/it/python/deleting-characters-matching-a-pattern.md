---
title:                "Eliminazione dei caratteri corrispondenti a un modello"
html_title:           "PowerShell: Eliminazione dei caratteri corrispondenti a un modello"
simple_title:         "Eliminazione dei caratteri corrispondenti a un modello"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
La cancellazione di caratteri che corrispondono a un pattern è un'operazione eseguita per rimuovere specifiche sequenze di caratteri da una stringa. Gli sviluppatori lo fanno per manipolare i dati, pulire il testo, fare parsing o implementare algoritmi di ricerca di stringhe.

## Come fare:
Per cancellare caratteri che corrispondono a un pattern in Python, utilizziamo il metodo translate() combinato con maketrans(). Ecco un esempio:

```Python 
s = 'Ciao, Mondo!'
pattern = ',!'
traslazione = str.maketrans('', '', pattern)
s = s.translate(traslazione)
print(s)  # Output: 'Ciao Mondo'
```
Questo codice rimuove le virgole e i punti esclamativi dalla stringa.

## Approfondimento:
La cancellazione di caratteri in base a un pattern è una pratica che risale alle prime fasi della programmazione. In Python, il metodo translate() è stato introdotto in Python 3.1, rendendo questa operazione più semplice e veloce.

Un'alternativa è l'uso di un loop for e una stringa temporanea per copiare i caratteri non desiderati. Tuttavia, questa soluzione è meno efficiente.

Dettagli di implementazione: maketrans() restituisce una tabella di traduzione utilizzabile per Unicode, utilizzata poi da translate() per sostituire i caratteri corrispondenti nella stringa.

## Vedi anche:
Per approfondire, visita questi collegamenti:
1. Documentazione Python sul metodo [translate()](https://docs.python.org/3/library/stdtypes.html#str.translate)
2. Documentazione Python sul metodo [maketrans()](https://docs.python.org/3/library/stdtypes.html#str.maketrans)
3. Articolo GitHub sulle [operazioni con le stringhe in Python](https://github.com/trekhleb/learn-python/tree/master/src/data_structures/string)
4. Post del blog [TutorialsPoint](https://www.tutorialspoint.com/python/string_translate.htm) sul metodo translate() in Python.