---
title:                "Ricerca e sostituzione del testo"
html_title:           "Arduino: Ricerca e sostituzione del testo"
simple_title:         "Ricerca e sostituzione del testo"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Eseguiamo la ricerca e la sostituzione del testo in Python

## Cosa & Perché?

La ricerca e la sostituzione del testo permettono di individuare una specifica sequenza di caratteri all'interno di una stringa e di sostituirla con un'altra sequenza. Questo è frequentemente richiesto dai programmatori per manipolare i dati di testo e per l'automazione.

## Come fare:

Per cercare e sostituire il testo in Python, utilizzeremo il metodo `replace()` della stringa. Ecco un esempio:

```Python
testo = "Hello, mondi!"
nuovo_testo = testo.replace("mondi", "mondo")
print(nuovo_testo)
```

Risultato:

```Python
Hello, mondo!
```

Questo codice sostituisce la parola "mondi" con "mondo."

## Approfondimento:

La funzione `replace()` è disponibile in Python fin dalla sua prima versione. Oltre a `replace()`, potrete utilizzare anche espressioni regolari (regex) per compiti di ricerca e sostituzione più complessi.

Ad esempio, per rimuovere tutte le vocali da una stringa:

```Python
import re
testo = "Hello, mondo!"
nuovo_testo = re.sub('[aeiou]', '', testo, flags=re.IGNORECASE)
print(nuovo_testo)
```

Risultato:

```Python
Hll, mnd!
```

La funzione `re.sub(pattern, repl, string)` cerca nel testo l’espressione regolare definita da `pattern`, la sostituisce con `repl` e restituisce il risultato.

## Vedi anche:

- Documentazione ufficiale di Python per le [stringhe](https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str) 
- Documentazione ufficiale di Python per le [espressioni regolari](https://docs.python.org/3/library/re.html) 
- Tutorial più approfonditi sulla [manipolazione delle stringhe in Python](https://realpython.com/python-strings/) 
- Esempi di utilizzo delle [espressioni regolari in Python](https://www.w3schools.com/python/python_regex.asp)