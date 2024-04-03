---
date: 2024-01-20 17:47:57.121813-07:00
description: 'Come fare: Ecco come ottenere la lunghezza di una stringa in Python,
  semplice e veloce.'
lastmod: '2024-03-13T22:44:42.992296-06:00'
model: gpt-4-1106-preview
summary: Ecco come ottenere la lunghezza di una stringa in Python, semplice e veloce.
title: Trovare la lunghezza di una stringa
weight: 7
---

## Come fare:
Ecco come ottenere la lunghezza di una stringa in Python, semplice e veloce:

```python
testo = "Ciao a tutti!"
lunghezza = len(testo)
print(lunghezza)  # Output: 13
```

La funzione `len()` è tutto ciò di cui hai bisogno.

## Approfondimento
In Python, la funzione `len()` è un modo diretto per trovare la lunghezza di vari tipi di dati, inclusi le stringhe. Storicamente, `len()` è stata una delle prime funzioni introdotte in Python, per la sua universalità e utilità.

Ci sono metodi alternativi, ma meno diretti, per calcolare la lunghezza di una stringa. Potresti, per esempio, usare un ciclo `for` per iterare attraverso la stringa e contare i caratteri:

```python
testo = "Ciao a tutti!"
contatore = 0
for carattere in testo:
    contatore += 1
print(contatore)  # Output: 13
```

Tuttavia, questo metodo è molto meno efficiente di `len()`. Nella sua implementazione, Python calcola la lunghezza di una stringa in tempo O(1), cioè in un tempo costante che non dipende dalla lunghezza della stringa, poiché la lunghezza della stringa è salvata all'interno dell'oggetto stringa.

## Vedi anche
- Documentazione ufficiale di Python sul tipo di dato stringa: https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str
- Tutorial Python per principianti: https://www.python.org/about/gettingstarted/
- Python Wiki su stringhe: https://wiki.python.org/moin/StringFAQ
