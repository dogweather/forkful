---
title:                "Python: Eliminare i caratteri corrispondenti a un modello"
simple_title:         "Eliminare i caratteri corrispondenti a un modello"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

A volte, durante la scrittura di codice in Python, potresti avere la necessità di eliminare determinati caratteri all'interno di una stringa che corrispondono ad un determinato pattern. Ciò potrebbe essere dovuto alla necessità di ripulire i dati o di filtrare informazioni indesiderate. In questo articolo, ti mostreremo come eliminare i caratteri che corrispondono ad un pattern in Python.

## Come

Utilizzare la funzione `re.sub()` del modulo `re` per eliminare i caratteri che corrispondono ad un pattern è abbastanza semplice. Di seguito ti forniamo un esempio di codice che mostra come eliminare tutti i numeri presenti in una stringa:

```Python
import re
testo = "Questa è una stringa con 123 numeri!"
nuovo_testo = re.sub(r"\d+", "", testo)
print(nuovo_testo)
```

Questo codice produrrà l'output:

```
Questa è una stringa con numeri!
```

Come puoi vedere, la funzione `re.sub()` sostituisce tutti i match del pattern (in questo caso, i numeri) con una stringa vuota. Il primo argomento della funzione è il pattern da cercare, mentre il secondo argomento è il testo in cui cercarlo.

Puoi utilizzare anche le espressioni regolari per identificare pattern più complessi. Ad esempio, se vuoi eliminare tutti i caratteri non alfanumerici da una stringa, puoi utilizzare questo codice:

```Python
import re
testo = "Questa è una stringa con caratteri non alfanumerici #!&?"
nuovo_testo = re.sub(r"[^\w\s]", "", testo)
print(nuovo_testo)
```

L'output sarà:

```
Questa è una stringa con caratteri non alfanumerici 
```

In questo caso, il pattern utilizzato è `[^\w\s]`, che corrisponde a tutti i caratteri non alfanumerici e non spazi. Quindi, la funzione sostituisce questi caratteri con una stringa vuota, eliminandoli dalla stringa originale.

## Deep Dive

La funzione `re.sub()` è molto utile quando si lavora con stringhe che contengono informazioni strutturate in un determinato formato. Utilizzando le espressioni regolari, è possibile identificare e eliminare facilmente parti indesiderate del testo.

Un'altra cosa importante da sapere è che `re.sub()` può accettare una funzione come terzo argomento, che viene utilizzata per sostituire i match del pattern con un valore personalizzato. Ad esempio, se vuoi sostituire tutti i numeri all'interno di una stringa con il loro doppio, puoi usare il seguente codice:

```Python
import re

def raddoppia(match):
    return str(int(match.group(0)) * 2)

testo = "Questa è una stringa con 123 numeri!"
nuovo_testo = re.sub(r"\d+", raddoppia, testo)
print(nuovo_testo)
```

La funzione `raddoppia()` viene utilizzata per manipolare il risultato del match prima di sostituirlo nella stringa. In questo caso, il match (il numero racchiuso nella stringa) viene convertito in intero, moltiplicato per 2 e poi convertito nuovamente in stringa prima di essere sostituito.

L'output sarà:

```
Questa è una stringa con 246 numeri!
```

Questa è solo una delle molte possibilità offerte dalla funzione `re.sub()` e dalle espressioni regolari per manipolare e filtrare stringhe in Python. Continua ad esplorare e prova diverse soluzioni per le tue esigenze specifiche.

## Vedi Anche

- Documentazione ufficiale del modulo `re`: https://docs.python.org/3/library/re.html
- Tutorial su espressioni regolari in Python: https://www.geeksforgeeks.org/python-regex-tutorial/
- Esercizi interattivi per praticare le espressioni regolari: https://regexone.com/