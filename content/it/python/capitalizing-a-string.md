---
title:    "Python: Capitalizzazione di una stringa"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Perché

Ci sono molte situazioni in cui potresti aver bisogno di capitalizzare una stringa nel tuo programma Python. Ad esempio, potresti dover formattare correttamente il nome di un utente o semplicemente voler rendere più leggibile un output.

## Come fare

Per capitalizzare una stringa in Python, puoi utilizzare il metodo `capitalize()` che è disponibile per tutti gli oggetti di tipo stringa. Ecco un esempio di come utilizzarlo:

```Python
testo = "questa è una stringa di esempio"
print(testo.capitalize())
```

Questo codice stamperà "Questa è una stringa di esempio" come output. Puoi anche utilizzare il metodo su una variabile contenente una stringa per aggiornare direttamente la stringa stessa.

```Python
nome = "marco"
nome = nome.capitalize()
print(nome)
```

Questo codice stamperà "Marco" come output.

## Approfondimento

Il metodo `capitalize()` in realtà non modifica la stringa originale, ma restituisce una nuova stringa con la prima lettera maiuscola e le rimanenti lettere minuscole. Se vuoi effettivamente modificare la stringa originale, puoi utilizzare il metodo `title()`, che restituirà una stringa in cui ogni parola inizia con una lettera maiuscola.

Inoltre, puoi utilizzare il metodo `upper()` per convertire una stringa interamente in maiuscolo o `lower()` per convertirla in minuscolo.

## Vedi anche

- [Documentazione ufficiale di Python sulle stringhe](https://docs.python.org/3/library/string.html)
- [Tutorial su HowToPython.org su stringhe in Python](https://howtopython.org/it/basico/stringhe/) 
- [Un approfondimento sulle stringhe in Python](https://realpython.com/python-strings/)

Grazie per aver letto questo articolo sul come capitalizzare una stringa in Python! Spero ti sia stato utile per migliorare le tue conoscenze di programmazione in Python. Ricorda sempre che la pratica e la sperimentazione sono fondamentali per diventare un buon programmatore. Felice codice!