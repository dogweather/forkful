---
title:                "Estrazione di sottostringhe"
html_title:           "Python: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Estrarre sottostringhe è una pratica comune tra i programmatori che consiste nell'ottenere una parte specifica di una stringa più grande. Questo può essere utile per una varietà di ragioni, ad esempio per analizzare e manipolare dati o per stampare solo una parte di una stringa.

## Come fare:

Ecco un esempio di codice Python per estrarre una sottostringa utilizzando la notazione degli indici:

```Python
stringa = "Ciao, come stai?"
print(stringa[0:4])

# Output: Ciao
```

Una cosa importante da notare è che l'indice del primo carattere è 0, quindi per estrarre la prima lettera di una stringa è necessario utilizzare 0 come indice. In questo esempio, abbiamo utilizzato [0:4] per estrarre i primi 4 caratteri della stringa.

Ci sono anche altre due notazioni comuni per estrarre sottostringhe: la notazione di slicing e la notazione di enumerazione inversa.

La notazione di slicing consiste nell'utilizzare la sintassi [inizio:fine:passo] per estrarre una parte specifica di una stringa. Ad esempio:

```Python
stringa = "Sono un programmatore"
print(stringa[5:12:2])

# Output: nu r
```

La notazione di enumerazione inversa, invece, utilizza indici negativi per estrarre una sottostringa partendo dalla fine della stringa. Ad esempio:

```Python
stringa = "Metodo di enumerazione inversa"
print(stringa[-5:])

# Output: versa
```

## Approfondimento:

L'idea di estrarre sottostringhe dalle stringhe risale ai primi linguaggi di programmazione come il Fortran e il Basic. Era un modo efficiente per accedere a una specifica parte dei dati all'interno di una stringa senza doverla manipolare completamente.

Oltre all'utilizzo della sintassi degli indici, ci sono anche altri modi per estrarre sottostringhe in Python, come l'utilizzo di espressioni regolari o delle funzioni predefinite come split() e replace().

Se sei interessato a saperne di più sui metodi per manipolare le stringhe in Python, puoi consultare la documentazione ufficiale o esplorare altri articoli e tutorial.

## Vedi anche:

- [Documentazione ufficiale di Python](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [Tutorial su come utilizzare le espressioni regolari in Python](https://www.w3schools.com/python/python_regex.asp)
- [Guida per principianti su come manipolare le stringhe in Python](https://realpython.com/python-strings/)