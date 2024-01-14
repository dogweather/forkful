---
title:    "Python: Trasformare una stringa in minuscolo"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

La conversione di una stringa in lettere minuscole è una praticità fondamentale in programmazione che permette di uniformare i dati e semplificare le operazioni di confronto. Inoltre, molte librerie e funzioni richiedono l'utilizzo di stringhe in minuscolo come argomento.

## Come Fare

```Python
# Definiamo una stringa in maiuscolo
testo = "QUESTA STRINGA E' IN MAIUSCOLO"

# Utilizzando il metodo lower(), possiamo convertirla in minuscolo
print(testo.lower())

# Output: questa stringa e' in minuscolo
```

## Approfondimento

La funzione lower() è uno dei tanti metodi disponibili per manipolare le stringhe in Python. Ciò che rende questa operazione particolarmente importante è che le stringhe sono immutabili, il che significa che non possono essere modificate una volta create. Quindi, per cambiare una stringa in minuscolo, dobbiamo creare una nuova stringa con il metodo lower().

Inoltre, è importante notare che la conversione in lettere minuscole è basata sul sistema di codifica ASCII, quindi alcuni caratteri speciali potrebbero non essere convertiti correttamente. In caso di dubbi, è possibile consultare la documentazione di Python per trovare una soluzione adeguata.

## Vedi Anche

- Documentazione di Python sulla funzione lower(): https://docs.python.org/3/library/stdtypes.html#str.lower 
- Tutorial di W3Schools sulla manipolazione delle stringhe in Python: https://www.w3schools.com/python/python_strings.asp