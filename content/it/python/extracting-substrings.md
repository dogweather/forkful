---
title:    "Python: Estrazione di sottostringhe"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

L'estrazione di sottostringhe può essere una tecnica utile da utilizzare in molti casi di programmazione in Python. Ad esempio, potresti voler estrarre una parte specifica di una stringa più lunga o effettuare ricerche più precise all'interno di una stringa.

## Come fare

Per estrarre una sottostringa in Python, puoi utilizzare il metodo `slice()` sull'oggetto stringa desiderato. Questo metodo accetta due argomenti: l'indice di inizio e l'indice di fine della sottostringa desiderata.

```
Python
test_string = "Questo è un esempio di stringa."
print(test_string[6:15])

# Output: è un ese
```

Puoi anche utilizzare il metodo `find()` per trovare una sottostringa all'interno di un'altra stringa e restituire l'indice iniziale della sottostringa desiderata.

```
Python
test_string = "Questo è un esempio di stringa."
print(test_string.find("esempio"))

# Output: 10
```

## Approfondimento

L'estrazione di sottostringhe può essere un'operazione molto utile quando si lavora con dati di testo. Ad esempio, puoi utilizzarla per manipolare URL, per ricerca di parole chiave o per filtrare dati.

## Vedi anche

- Tutorial su come utilizzare la funzione `slice()` in Python: https://www.programiz.com/python-programming/methods/built-in/slice
- Guida pratica su come utilizzare il metodo `find()` in Python: https://www.geeksforgeeks.org/python-string-find/