---
title:                "Python: Estrazione di sottostringhe"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore Python, probabilmente hai familiarità con il concetto di stringhe, ovvero una sequenza di caratteri. Ma a volte può essere necessario estrarre una parte specifica di una stringa, chiamata sottosequenza o sottotesto. In questo articolo, esploreremo come eseguire questa operazione utilizzando il linguaggio di programmazione Python.

## Come fare

Per estrarre una sottosequenza da una stringa in Python, possiamo utilizzare il "slicing" o l'indicizzazione delle stringhe. Slicing significa tagliare una parte di una stringa in base alla sua posizione nella sequenza. Ad esempio, se abbiamo una stringa "Python", possiamo estrarre la sottosequenza "yth" utilizzando `[1:4]`. Questo indicatore di posizione ci dice di iniziare dall'indice 1 (che rappresenta il carattere "y") e di terminare all'indice 4 (che rappresenta l'ultimo carattere "n" escluso).

Vediamo un esempio in codice:

```python
stringa = "Python"
sottosequenza = stringa[1:4]
print(sottosequenza)

# Output: yth
```

Inoltre, possiamo anche specificare la lunghezza del passo di slicing, ovvero quanti elementi vengono saltati durante il processo di estrazione. Ad esempio, se vogliamo estrarre ogni secondo carattere della stringa "Python", possiamo usare `[::2]` come segue:

```python
stringa = "Python"
sottosequenza = stringa[::2]
print(sottosequenza)

# Output: Pto
```

Oltre all'utilizzo di uno slicing manuale, Python ci offre anche il metodo `slice()` che possiamo utilizzare per creare un oggetto di slice e applicarlo alla nostra stringa. Esempio:

```python
stringa = "Python"
oggetto_slice = slice(1, 4) # Uguale a [1:4]
sottosequenza = stringa[oggetto_slice]
print(sottosequenza)

# Output: yth
```

## Approfondimento

Ora che abbiamo capito come estrarre le sottosequenze dalle stringhe, vediamo alcune applicazioni pratiche. Ad esempio, possiamo utilizzare questo concetto per rimuovere i caratteri non desiderati da una stringa. Invece di utilizzare il metodo `replace()` che sostituisce i caratteri specificati con una stringa vuota, possiamo usare il slicing per rimuovere una porzione di caratteri.

Oltre a ciò, possiamo anche utilizzare il slicing per verificare se una stringa è palindroma. Un palindromo è una parola o una frase che si legge uguale da sinistra a destra e da destra a sinistra. Ad esempio, "radar" è un palindromo. Utilizzando il slicing, possiamo confrontare la stringa originale con la sua versione invertita e verificare se sono uguali.

## Vedi anche

- [Documentazione ufficiale di Python su slicing](https://docs.python.org/3/library/functions.html#slice)
- [Esempi di slicing in Python](https://www.programiz.com/python-programming/methods/string/slice) 
- [Tutorial su come invertire una stringa in Python utilizzando slicing](https://www.askpython.com/python/string/reverse-string-python-using-slice)