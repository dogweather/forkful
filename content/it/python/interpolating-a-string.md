---
title:                "Interpolazione di una stringa"
html_title:           "Python: Interpolazione di una stringa"
simple_title:         "Interpolazione di una stringa"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Interpolare una stringa significa inserire dei valori all'interno di una stringa già esistente. I programmatori lo fanno per rendere il codice più efficiente, dinamico e leggibile.

## Come fare:

Per interpolare una stringa in Python, possiamo utilizzare il metodo `format()` o la sintassi delle "stringhe f" (lettera "f" precedente alle virgolette del testo). Ecco un esempio utilizzando `format()`:

```Python
name = "Giorgio"
age = 25
message = "Ciao, mi chiamo {} e ho {} anni".format(name, age)
print(message)
```

Output: Ciao, mi chiamo Giorgio e ho 25 anni

Ecco invece un esempio utilizzando la sintassi delle "stringhe f":

```Python
name = "Giorgio"
age = 25
message = f"Ciao, mi chiamo {name} e ho {age} anni"
print(message)
```

Output: Ciao, mi chiamo Giorgio e ho 25 anni

## Approfondimento:

L'interpolazione di stringhe è diventata popolare con il linguaggio di programmazione Perl negli anni '90, ma è stata poi implementata anche in altri linguaggi, tra cui Python. Un'alternativa all'interpolazione di stringhe è l'utilizzo di concatenazioni di stringhe, ma questa soluzione può risultare meno leggibile e più soggetta ad errori.

Per quanto riguarda l'implementazione dell'interpolazione di stringhe in Python, il metodo `format()` funziona passando i valori da inserire come argomenti tra parentesi graffe all'interno della stringa. Con la sintassi delle "stringhe f", invece, i valori vengono inseriti direttamente tra parentesi graffe all'interno della stringa senza bisogno di utilizzare il metodo `format()`.

## Vedi anche:

- Documentazione ufficiale di Python su string formatting: https://docs.python.org/3/library/string.html#format-string-syntax 
- Tutorial su string formatting in Python: https://realpython.com/python-string-formatting/