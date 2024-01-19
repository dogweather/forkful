---
title:                "Capitalizzare una stringa"
html_title:           "Python: Capitalizzare una stringa"
simple_title:         "Capitalizzare una stringa"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Mettere la Prima Lettera Maiuscola in una Stringa con Python

## Cos'è e Perché?
Mettere la prima lettera maiuscola in una stringa significa trasformare la prima lettera di ogni parola in maiuscolo, lasciando in minuscolo il resto. Questo è comune nella programmazione per migliorare la leggibilità e l'aspetto estetico del testo.

## Come si Fa:
È molto semplice farlo in Python usando il metodo `title()`. Ecco un esempio:

```Python
# Creiamo una stringa semplice.
frase = "ciao mondo"
print(frase.title())
```
L'output sarà:

```
Ciao Mondo
```

## Approfondimenti
La funzione `title()` è in Python fin dalla sua prima versione. È una delle tante funzioni per il trattamento delle stringhe, insieme a `lower()`, `upper()`, e `swapcase()`, tra gli altri.

Un'alternativa potrebbe essere utilizzare un ciclo for per capitalizzare ogni parola in una stringa, ma sarebbe meno efficiente e più ingombrante.

La funzione `title()` in Python funziona dividendo la stringa in parole con il metodo `split()`, rendendo maiuscola la prima lettera di ogni parola con il metodo `capitalize()`, e poi unendo insieme le parole con il metodo `join()`.

## Vedi Anche
Per ulteriori informazioni sulle funzioni delle stringhe in Python, vedi:

* Documentazione ufficiale Python: [https://docs.python.org/3/library/stdtypes.html#string-methods](https://docs.python.org/3/library/stdtypes.html#string-methods)
* Tutorial su Real Python: [https://realpython.com/python-strings/](https://realpython.com/python-strings/)
* Guide su W3Schools: [https://www.w3schools.com/python/python_strings.asp](https://www.w3schools.com/python/python_strings.asp)