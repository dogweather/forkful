---
title:    "Python: Eliminazione dei caratteri corrispondenti ad un modello"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Perché

Spesso, durante la scrittura di codice in Python, potremmo trovarci nella situazione in cui abbiamo bisogno di eliminare determinati caratteri da una stringa. Questo può essere dovuto a vari motivi: ripulire dati, rimuovere caratteri non validi o semplicemente per ottenere una stringa più pulita. In questi casi, potrebbe essere utile conoscere il processo di eliminazione dei caratteri in base a un modello predefinito.

## Come fare

In Python, possiamo utilizzare il metodo `.strip()` per eliminare i caratteri all'inizio e alla fine di una stringa, ma questo non ci permette di specificare un modello preciso da seguire. Per questo, possiamo utilizzare la funzione `re.sub()` del modulo "re" per eliminare i caratteri in base a un'espressione regolare.

Un esempio di codice potrebbe essere questo:

```python
import re

stringa = "123abcXYZ$!"

output = re.sub("[a-z, A-Z]", "", stringa)
print(output) # Output: 123$!
```

In questo esempio, stiamo utilizzando l'espressione regolare `[a-z, A-Z]` per selezionare tutti i caratteri alfabetici, e poi li sostituiamo con una stringa vuota, ottenendo così una stringa senza caratteri alfabetici.

## Approfondimento

Esaminiamo più da vicino l'utilizzo di `re.sub()` per eliminare i caratteri in base a un modello. La sintassi di base è la seguente:

```python
re.sub(pattern, repl, stringa)
```

Dove "pattern" rappresenta l'espressione regolare che corrisponde ai caratteri che vogliamo eliminare, "repl" è ciò che vogliamo sostituire ai caratteri corrispondenti e "stringa" è la stringa in cui vogliamo effettuare la sostituzione.

Possiamo anche utilizzare l'argomento opzionale "count" per specificare il numero massimo di sostituzioni che vogliamo effettuare. Per esempio:

```python
output = re.sub("[a-z]", "X", stringa, count=2)
print(output) # Output: 123XbcXYZ$!
```

In questo caso, stiamo limitando il numero di sostituzioni a 2, quindi solo i primi due caratteri alfabetici verranno sostituiti con "X".

## Vedi anche

- Documentazione ufficiale di Python su `re.sub()`: https://docs.python.org/3/library/re.html#re.sub
- Tutorial sulle espressioni regolari in Python: https://www.dataquest.io/blog/regex-cheatsheet/