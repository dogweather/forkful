---
title:                "Incorporare stringhe"
html_title:           "Python: Incorporare stringhe"
simple_title:         "Incorporare stringhe"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

Molte volte, quando si programma in Python, è necessario unire diverse stringhe per formare una nuova stringa. L'operazione di concatenazione di stringhe è fondamentale per la manipolazione dei dati e può essere utile in molte situazioni diverse.

## Come fare

Per concatenare stringhe in Python, è possibile utilizzare l'operatore `+` o il metodo built-in `format()`.

```
stringa1 = "Ciao"
stringa2 = "mondo!"

# utilizzando l'operatore +
print(stringa1 + " " + stringa2)
# output: Ciao mondo!

# utilizzando il metodo format()
print("{} {}".format(stringa1, stringa2))
# output: Ciao mondo!
```

Il metodo `format()` ha la possibilità di specificare il formato delle stringhe concatenate, come ad esempio l'allineamento o il numero di cifre decimali per i numeri.

```
numero1 = 1234
numero2 = 56.789

# specificando l'allineamento con :<
print("{:<10} {:<10}".format(numero1, numero2))
# output: 1234       56.789    

# specificando il numero di cifre decimali con :.2f
print("Il valore di numero2 è {:.2f}".format(numero2))
# output: Il valore di numero2 è 56.79
```

## Approfondimento

In Python, le stringhe sono immutabili, cioè non possono essere modificate. Quindi, ogni volta che si esegue un'operazione di concatenazione, in realtà si crea una nuova stringa. Questo potrebbe portare ad un accumulo di memoria e problemi di prestazioni se utilizzato in modo consistente su grandi stringhe.

Per evitare questo problema, si consiglia di utilizzare il metodo `join()` invece di concatenare le stringhe con l'operatore `+`. Il metodo `join()` accetta una lista di stringhe e le unisce in una sola stringa.

```
lista_nomi = ["Maria", "Luca", "Giulia"]
# utilizzando l'operatore +
greeting = "Ciao " + lista_nomi[0] + ", " + lista_nomi[1] + ", " + lista_nomi[2]
# output: Ciao Maria, Luca, Giulia

# utilizzando il metodo join()
greeting = "Ciao " + ", ".join(lista_nomi)
# output: Ciao Maria, Luca, Giulia
```

## Vedi anche

- Documentazione ufficiale Python su stringhe: https://docs.python.org/3/library/stdtypes.html#string-methods
- Tutorial su concatenazione e manipolazione delle stringhe in Python: https://realpython.com/python-strings/