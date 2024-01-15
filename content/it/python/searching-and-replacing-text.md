---
title:                "Ricerca e sostituzione di testo"
html_title:           "Python: Ricerca e sostituzione di testo"
simple_title:         "Ricerca e sostituzione di testo"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

C'è una cosa che tutti gli sviluppatori devono fare regolarmente: cercare e sostituire del testo. Può sembrare noioso, ma è un'operazione essenziale per mantenere il proprio codice efficiente e pulito.

## Come

Fortunatamente, Python ha alcune funzioni integrate che semplificano questa attività. Per iniziare, abbiamo bisogno di comprendere la funzione `replace()`. Vediamo un esempio:

```Python
# Creiamo una stringa di esempio
stringa = "Questo è un esempio di stringa che dobbiamo modificare."

# Utilizziamo la funzione replace() per sostituire "esempio" con "esercizio"
nuova_stringa = stringa.replace("esempio", "esercizio")

# Stampiamo il risultato
print(nuova_stringa)
```

Output:
```
Questo è un esercizio di stringa che dobbiamo modificare.
```

Come puoi vedere, la funzione `replace()` prende due argomenti: il testo da sostituire e il nuovo testo da utilizzare al suo posto. È importante notare che la funzione sostituirà solo il primo corrispondente che trova nella stringa.

Se vogliamo sostituire tutte le occorrenze del testo, possiamo utilizzare il metodo `replace()` all'interno di un ciclo `while`:

```Python
# Creiamo una stringa di esempio
stringa = "Questo è un esempio di stringa che dobbiamo modificare."

# Utilizziamo il ciclo while per sostituire tutte le occorrenze di "esempio" con "esercizio"
while "esempio" in stringa:
    stringa = stringa.replace("esempio", "esercizio")

# Stampiamo il risultato
print(stringa)
```

Output:
```
Questo è un esercizio di stringa che dobbiamo modificare.
```

In questo caso, il ciclo continuerà fino a quando non troverà più occorrenze del testo da sostituire nella stringa.

## Deep Dive

Oltre alla funzione `replace()`, Python ha anche un modulo chiamato `re` (regular expressions) che ci permette di utilizzare espressioni regolari per cercare e sostituire testo in modo più avanzato. Questo modulo offre una maggiore flessibilità e controllo nella sostituzione del testo.

Ecco un esempio di come potremmo utilizzare il modulo `re` per sostituire tutto il testo compreso tra due punti esclamativi con un testo personalizzato:

```Python
import re

# Creiamo una stringa di esempio
stringa = "Questo è un esempio di stringa! Che utilizzeremo per la sostituzione? Wonderful!"

# Utilizziamo la funzione sub() del modulo re per sostituire tutto il testo tra i punti esclamativi con "Ottimo lavoro!"
nuova_stringa = re.sub(r'!(.*?)!', r'Ottimo lavoro!', stringa)

# Stampiamo il risultato
print(nuova_stringa)
```

Output:
```
Questo è un esempio di stringa! Ottimo lavoro! Wonderful!
```

Il codice sopra utilizza una "espressione regolare" (rappresentata da `r'!(.*?)!'`) per identificare il testo che ci interessa sostituire. Ciò significa che sostituirà tutto ciò che si trova tra i due punti esclamativi con la stringa "Ottimo lavoro!".

## See Also

- [Documentation di Python sul metodo replace()](https://docs.python.org/3/library/stdtypes.html?highlight=replace#str.replace)
- [Documentazione di Python sul modulo re](https://docs.python.org/3/library/re.html)