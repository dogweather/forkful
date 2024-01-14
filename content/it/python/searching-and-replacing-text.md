---
title:                "Python: Ricerca e sostituzione di testo"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

C'è un motivo per cui la ricerca e la sostituzione di testo sono strumenti così fondamentali per i programmatori Python. Non solo semplificano il processo di manipolazione dei dati, ma aumentano anche l'efficienza e riducono al minimo gli errori umani.

## Come fare

La sintassi di base per la ricerca e la sostituzione di testo in Python è la seguente:

```Python
stringa = "Questo è il mio blog su Python."

# Sostituzione di una parola all'interno della stringa
nuova_stringa = stringa.replace("blog", "post")

# Output: "Questo è il mio post su Python."
print(nuova_stringa)

# Sostituzione di un carattere all'interno della stringa
nuova_stringa2 = stringa.replace("mio", "nostro")

# Output: "Questo è il nostro blog su Python."
print(nuova_stringa2)
```

La funzione `.replace()` accetta due argomenti: la parola o il carattere da sostituire e la parola o il carattere di sostituzione. È importante notare che questa operazione non modifica la stringa originale, ma restituisce una nuova stringa con la sostituzione effettuata.

## Approfondimento

Esistono alcune varianti della funzione `.replace()` che possono essere utili in diverse situazioni. Ad esempio, è possibile specificare il numero massimo di sostituzioni da effettuare:

```Python
stringa = "Il mio nome è Giorgio e ho 29 anni."
nuova_stringa = stringa.replace("o", "a", 1)

# Output: "Il mia nome è Giorgio e ho 29 anni."
print(nuova_stringa)
```

In questo esempio, la prima lettera "o" viene sostituita con la lettera "a", ma solo una volta. Se vogliamo effettuare tutte le sostituzioni possibili, possiamo passare l'argomento opzionale `count`, impostato su -1:

```Python
stringa = "Il mio nome è Giorgio e ho 29 anni."
nuova_stringa = stringa.replace("o", "a", -1)

# Output: "Il mia nome è Giargia a ha 29 anni."
print(nuova_stringa)
```

Un'altra funzione utile è `.strip()`, che rimuove i caratteri specificati all'inizio e alla fine di una stringa. Ad esempio:

```Python
stringa = "   Ciao, mi chiamo Luca!   "
nuova_stringa = stringa.strip(" ")

# Output: "Ciao, mi chiamo Luca!"
print(nuova_stringa)
```

È possibile anche concatenare più funzioni per effettuare operazioni complesse di ricerca e sostituzione, come ad esempio:

```Python
stringa = "Il mio nome è Mario e ho 22 anni."
nuova_stringa = stringa.replace("Mario", "Luca").strip(" ").lower()

# Output: "il mio nome è luca e ho 22 anni."
print(nuova_stringa)
```

## Vedi anche
- [Documentazione ufficiale di Python sulla funzione `replace()`](https://docs.python.org/3/library/stdtypes.html#str.replace)
- [Tutorial su Python per principianti](https://realpython.com/python-beginner-tips/) 
- [Tutorial avanzato su manipolazione di stringhe in Python](https://realpython.com/python-strings/)