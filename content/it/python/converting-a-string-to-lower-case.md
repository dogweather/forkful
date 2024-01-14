---
title:                "Python: Conversione di una stringa in minuscolo"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Convertire una stringa in minuscolo è un'operazione molto comune nella programmazione Python. Questo è utile quando si vuole confrontare due stringhe o quando si vuole uniformare il formato delle parole all'interno di un testo.

## Come fare 

```Python
parola = "CIAO MONDO"

print(parola.lower()) # output: ciao mondo 
```

Nell'esempio sopra, abbiamo definito una variabile `parola` con il valore "CIAO MONDO". Utilizzando il metodo `lower()`, abbiamo convertito il valore della variabile in minuscolo e poi lo abbiamo stampato a schermo. È importante notare che il metodo `lower()` non modifica il valore della variabile, ma ne crea una nuova con la stringa convertita. 

Un'altra forma di convertire una stringa in minuscolo è utilizzare la funzione `str.casefold()`. Questa funzione è simile al metodo `lower()`, ma è più efficace nel gestire caratteri non ASCII, come ad esempio quelli con accenti.

```Python
parola = "È BELLO IL SOLE"

print(str.casefold(parola)) # ouput: è bello il sole
```

## Approfondimento

La conversione di una stringa in minuscolo può essere influenzata dalla localizzazione del sistema operativo. Ad esempio, su sistemi con impostazioni di localizzazione diverse, la conversione di una stringa con caratteri accentati può produrre risultati diversi. È importante tenere in conto di questo quando si sviluppano applicazioni che verranno utilizzate in diverse lingue.

Inoltre, è possibile specificare manualmente le lettere dell'alfabeto che dovranno essere convertite in minuscolo, passandole come argomento alla funzione `str.casefold()`. Ad esempio, per convertire una stringa con caratteri greci in minuscolo, si può utilizzare la seguente sintassi:

```Python
stringa_greca = "ΓΕΙΑ ΣΟΥ ΚΟΣΜΕ"

print(str.casefold(stringa_greca, "el")) # output: γεια σου κοσμε
```

## Vedi anche

- Documentazione Python sulle stringhe: https://docs.python.org/3/library/stdtypes.html#str.lower
- Informazioni sulla funzione str.casefold: https://docs.python.org/3/library/stdtypes.html#str.casefold