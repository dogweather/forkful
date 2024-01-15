---
title:                "Stampa della produzione di debug"
html_title:           "Python: Stampa della produzione di debug"
simple_title:         "Stampa della produzione di debug"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perché

Il debugging è una parte essenziale della programmazione e la stampa dei messaggi di debug è uno strumento utile per trovare ed eliminare gli errori nel codice. Questo articolo vi mostrerà come utilizzare la stampa di output di debug in Python per semplificare il processo di debugging.

## Come fare

Per stampare un messaggio di debug in Python, è possibile utilizzare la funzione `print()`. Questa funzione accetta uno o più argomenti e li stampa a schermo separati da uno spazio.

```Python
nome = "Maria"
cognome = "Rossi"
print("Il nome completo è:", nome, cognome)
```

Output:
`Il nome completo è: Maria Rossi`

Inoltre, è possibile utilizzare la formattazione di stringhe per rendere più dinamico il messaggio di debug. Questo può essere fatto utilizzando il metodo `.format()` o i f-strings (disponibili a partire da Python 3.6).

```Python
numero = 25
print("Il numero inserito è: {}".format(numero))
```

Output:
`Il numero inserito è: 25`

```Python
nome = "Giulio"
print(f"Ciao, mi chiamo {nome}")
```

Output:
`Ciao, mi chiamo Giulio`

La stampa di output di debug può anche essere utilizzata per controllare il flusso del programma, come ad esempio stampare dei messaggi durante l'esecuzione di un ciclo.

```Python
for numero in range(10):
    print("Il valore attuale è:", numero)
```

Output:
```
Il valore attuale è: 0
Il valore attuale è: 1
Il valore attuale è: 3
...
Il valore attuale è: 9
```

## Approfondimento

La stampa di output di debug è un metodo molto utile per identificare gli errori nel codice, ma è importante utilizzarlo correttamente. Eccessive stampe di debug possono appesantire il programma e rallentare l'esecuzione. Inoltre, è consigliabile utilizzare la funzione `logging` di Python invece della normale `print()` per avere un maggior controllo sui messaggi di debug.

## Vedi anche

- [Documentazione ufficiale Python su `print()`](https://docs.python.org/3/library/functions.html#print)
- [Approfondimenti su logging in Python](https://realpython.com/python-logging/)
- [Guida dettagliata alla formattazione di stringhe in Python](https://realpython.com/python-f-strings/)