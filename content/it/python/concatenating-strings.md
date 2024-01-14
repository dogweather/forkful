---
title:                "Python: Concatenazione di stringhe"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

La concatenazione delle stringhe è un'operazione fondamentale nella programmazione in Python che permette di unire più stringhe in una sola. Questo può essere utile per creare messaggi personalizzati o per formattare output in modo chiaro e leggibile.

## Come fare

Per concatenare stringhe in Python, è possibile utilizzare l'operatore "+" o il metodo "join()". Vediamo un esempio di codice utilizzando l'operatore "+":

```Python
# Definiamo due stringhe
stringa1 = "Ciao"
stringa2 = "mondo!"

# Concateniamo le due stringhe
nuova_stringa = stringa1 + " " + stringa2

# Stampiamo il risultato
print(nuova_stringa)

```

Output:

```
Ciao mondo!
```

Per utilizzare il metodo "join()", invece, è necessario creare una lista contenente le stringhe da concatenare. Vediamo un esempio:

```Python
# Definiamo una lista di parole
parole = ["Ciao", "mondo!"]

# Utilizziamo il metodo "join()"
nuova_stringa = " ".join(parole)

# Stampiamo il risultato
print(nuova_stringa)

```

Output:

```
Ciao mondo!
```

## Approfondimento

La concatenazione delle stringhe in Python è possibile grazie al concetto di "mutable" e "immutable". Le stringhe in Python sono "immutable", il che significa che non possono essere modificate direttamente. Invece, ogni volta che viene eseguita un'operazione di concatenazione, viene creata una nuova stringa.

Inoltre, è importante notare che il metodo "join()" è più efficiente rispetto all'utilizzo dell'operatore "+", soprattutto quando si lavora con grandi quantità di stringhe.

## Vedi anche

- [Documentazione ufficiale su stringhe in Python](https://docs.python.org/3/library/string.html)
- [Tutorial su concatenazione di stringhe in Python](https://realpython.com/python-strings/)
- [Domande frequenti su concatenazione di stringhe in Python](https://www.programiz.com/python-programming/methods/string/join)