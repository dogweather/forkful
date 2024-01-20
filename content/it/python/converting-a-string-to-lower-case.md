---
title:                "Convertire una stringa in minuscolo"
html_title:           "Arduino: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

---

## Cos'è & Perché?

Trasformare una stringa in minuscolo significa convertire tutti i caratteri di una stringa in lettere minuscole. Questo è fatto dai programmatori per facilitare il confronto tra stringhe e per standardizzare l'input dell'utente.

## Come fare:

```Python
# Stringa di esempio
str_esempio = "CIAO MONDO"

# Conversione a minuscolo utilizzando il metodo lower()
str_convertita = str_esempio.lower()

print(str_convertita)
```

L'output sarà:

```Python
"ciao mondo"
```

## Approfondimenti

Convertire una stringa in minuscolo è un concetto fondamentale della programmazione e presente in tutti i linguaggi, non solo in Python. Esistono alternative, come l'utilizzo di loop e condizioni, ma il metodo `.lower()` è il più semplice ed efficiente in Python.

La sua implementazione specifica può variare tra le diverse versioni di Python e i diversi sistemi operativi. Ad esempio, nelle implementazioni Unicode di Python, il metodo `.lower()` rispetta le regole di conversione definite nel Unicode Data File.

## Altri riferimenti

- [Documentazione ufficiale Python sul metodo `.lower()`](https://docs.python.org/3/library/stdtypes.html#str.lower)
- [Esempi Python String lower() su Programiz](https://www.programiz.com/python-programming/methods/string/lower) 

---