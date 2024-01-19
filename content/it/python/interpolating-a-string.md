---
title:                "Interpolazione di una stringa"
html_title:           "Clojure: Interpolazione di una stringa"
simple_title:         "Interpolazione di una stringa"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Che Cosa & Perché?
L'interpolazione di stringhe è un metodo utilizzato per incorporare espressioni variabili all'interno di stringhe. I programmatori lo fanno per creare stringhe dinamicamente e migliorare la leggibilità del codice.

## Come si fa:
Python fornisce due metodi principali per l'interpolazione di stringhe, utilizzando il metodo `format()` o le `f-string`. Ecco degli esempi:

```Python
# Metodi: `.format()`
nome = "Mario"
print("Ciao, {}!".format(nome))  # Output: "Ciao, Mario!"

# Metodi: `f-string`
nome = "Mario"
print(f"Ciao, {nome}!")  # Output: "Ciao, Mario!"
```

## Approfondimento
Una volta, l'interpolazione di stringhe era fatta attraverso l'operatore `%`, modello ispirato dalla sintassi di printf di C. Questo è ancora supportato in Python, ma è considerato obsoleto.

```Python
# Metodi: `%`
nome = "Mario"
print("Ciao, %s!" % nome)  # Output: "Ciao, Mario!"
```
Il metodo `.format()` è stato introdotto in Python 2.6 e ha migliorato la versatilità e la facilità d'uso rispetto all'operatore `%`.

Successivamente, con Python 3.6, è stato introdotto il sistema di `f-string`, che affronta alcuni limiti dei metodi precedenti e rende il codice ancora più leggibile.

Notate di essere consapevoli del fatto che tutti questi metodi coesistono in Python, ma le `f-string` sono generalmente preferite per la loro efficacia e leggibilità.

## Link Utili
1. [`String Formatting`](https://docs.python.org/3/library/stdtypes.html#str.format) nella documentazione ufficiale di Python.
2. [`f-strings`](https://docs.python.org/3/reference/lexical_analysis.html#f-strings) in Python 3.6+.
3. [PEP 3101](https://www.python.org/dev/peps/pep-3101/) - Una Nuova Era per la Formattazione di Stringhe in Python.
4. [PEP 498](https://www.python.org/dev/peps/pep-0498/) - Stringhe Letterali Formattate.