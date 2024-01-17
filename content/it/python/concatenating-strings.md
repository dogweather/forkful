---
title:                "Concatenazione di stringhe"
html_title:           "Python: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Cos'è e perché?

La concatenazione di stringhe in Python è il processo di unire due o più stringhe per crearne una più lunga. I programmatori spesso lo fanno quando vogliono creare una nuova stringa a partire da più piccole, come ad esempio unire il testo di una variabile con una stringa fissa.

## Come fare:

```Python
nome = 'Mario'
cognome = 'Rossi'
etichetta = 'Ciao, sono ' + nome + ' ' + cognome + '!'
print(etichetta)
```

Output:
```
Ciao, sono Mario Rossi!
```

## Approfondimento:

La concatenazione di stringhe è una funzionalità comune in molti linguaggi di programmazione. In Python, è possibile eseguire la concatenazione anche con il metodo .format() o con le f-string. Inoltre, è importante notare che le stringhe sono immutabili in Python, quindi ogni volta che si effettua la concatenazione, viene creato un nuovo oggetto stringa.

## Vedi anche:

- [Metodi di formattazione delle stringhe in Python](https://realpython.com/python-string-formatting/)
- [Documentazione ufficiale di Python sulla concatenazione di stringhe](https://docs.python.org/3/library/string.html#string-functions)