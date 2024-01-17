---
title:                "Convertire una stringa in minuscolo"
html_title:           "Python: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
La conversione di una stringa in minuscolo è un'operazione comune nel mondo della programmazione. In pratica, si tratta di trasformare tutte le lettere maiuscole presenti in una stringa in lettere minuscole. I programmatori lo fanno principalmente per uniformare la formattazione delle stringhe e semplificare le operazioni di confronto e ricerca.

## Come:
Basta utilizzare il metodo `lower()` sulla stringa desiderata per effettuare la conversione. Ad esempio:
```
stringa = "CIAO MONDO!"
print(stringa.lower())
```
Output: ciao mondo!

## Approfondimento:
La conversione di stringhe in minuscolo ha origini nella lingua inglese, dove il concetto di maiuscole e minuscole è molto più importante rispetto ad altre lingue. Negli anni sono stati sviluppati molti modi diversi di effettuare questa operazione, come utilizzare il modulo `string` e il metodo `casefold()`. In Python, il metodo `lower()` è la scelta più semplice ed efficace.

## Vedi anche:
- [Documentazione ufficiale di Python su stringhe](https://docs.python.org/3/library/string.html)
- [Differenze tra `lower()` e `casefold()`](https://www.geeksforgeeks.org/difference-between-casefold-and-lower-in-python/)