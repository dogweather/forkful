---
title:                "Python: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Converting una stringa in minuscolo è una delle operazioni più comuni in programmazione, utile quando si vuole manipolare o confrontare parole senza preoccuparsi delle maiuscole o minuscole. Questo può essere utile per creare filtri di ricerca o per verificare la correttezza ortografica.

## Come fare

```Python
stringa = "QUESTA È UNA STRINGA IN MAIUSCOLO"
stringa_minuscola = stringa.lower()
print(stringa_minuscola)
```

Output: questa è una stringa in maiuscolo

In Python, la funzione `lower()` è disponibile per ogni stringa, convertendo tutti i caratteri in minuscolo. È una soluzione semplice ed efficace per lavorare con parole in un formato unificato.

## Approfondimento

Рer capire come funziona realmente la conversione in minuscolo di una stringa in Python, è necessario analizzare il concetto di Unicode. Unicode è uno standard di codifica che assegna un valore numerico univoco a ogni carattere, simbolo o emoji. Ogni lettera maiuscola e minuscola è rappresentata da due numeri differenti.

Quando si utilizza la funzione `lower()` in Python, viene utilizzato l'elenco dei valori numerici delle lettere maiuscole e vengono convertiti in quelli delle lettere minuscole. Questo è possibile poiché esiste una relazione predefinita tra i due.

Ci sono però alcune eccezioni in cui questa regola non si applica, ad esempio per le lettere accentate o con segni diacritici. In questo caso, la conversione in minuscolo non è semplicemente una sostituzione del valore numerico, ma richiede una maggiore comprensione dei concetti di Unicode.

## Vedi anche

- [Documentazione ufficiale di Python per la funzione `lower()`](https://docs.python.org/3/library/stdtypes.html#str.lower)
- [Guida completa agli Unicode in Python](https://docs.python.org/3/howto/unicode.html)
- [Unicode e le sfide della codifica dei caratteri](https://blog.mozilla.org/nunjucks/2015/10/27/unicode-character-encoding-issues/)