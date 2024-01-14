---
title:    "Python: Ricerca e sostituzione di testo"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

La ricerca e sostituzione di testo è una pratica comune nella programmazione che permette di automatizzare e semplificare il processo di modifica di un testo. Con l'utilizzo di appositi strumenti e codice, è possibile trovare e rimpiazzare parole, frasi o caratteri in modo veloce ed efficiente.

## Come fare

Per eseguire una ricerca e sostituzione di testo in Python, utilizzeremo la funzione `replace()` che è disponibile per le stringhe, ovvero una sequenza di caratteri. Questa funzione accetta due parametri: il testo da cercare e il testo da sostituire.

Esempio di codice:

`````Python 
stringa = "Buonanotte a tutti"
nuova_stringa = stringa.replace("notte", "giorno")
print(nuova_stringa)
`````

Output:

```
Buonagiorno a tutti
```

## Approfondimento

Oltre alla funzione `replace()`, esistono altri metodi per effettuare una ricerca e sostituzione di testo in Python. Ad esempio, è possibile utilizzare le espressioni regolari, che permettono di cercare e sostituire pattern di testo più complessi.

Inoltre, è importante sottolineare che la funzione `replace()` effettua una sostituzione solo del primo match trovato. Se si desidera sostituire tutti i match presenti nella stringa, è necessario utilizzare il parametro opzionale `count` per specificare il numero di sostituzioni da effettuare.

## Vedi anche

- [Documentazione ufficiale di Python sulla funzione `replace()`](https://docs.python.org/3/library/stdtypes.html#str.replace)
- [Tutorial su espressioni regolari in Python](https://www.w3schools.com/python/python_regex.asp)
- [Risorse per l'apprendimento di Python](https://learntoprogram.ai/programming-with-python/)