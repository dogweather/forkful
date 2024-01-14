---
title:                "Haskell: Eliminare i caratteri corrispondenti a un determinato schema"
simple_title:         "Eliminare i caratteri corrispondenti a un determinato schema"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Ci sono diversi motivi per cui si potrebbe voler eliminare dei caratteri corrispondenti a un determinato modello. Ad esempio, potresti essere alle prese con dati corrotti o errati e voler ripulire il tuo dataset prima di utilizzarlo in una analisi o un'altra operazione. Oppure potresti avere una stringa di testo lunga e desiderare di eliminarne una parte che segue uno schema specifico.

## Come fare

Per eliminare i caratteri corrispondenti a un certo modello in Haskell, è possibile utilizzare la funzione `filter` combinata con l'operatore `(/=)`, che permette di filtrare una lista mantenendo solo gli elementi che non soddisfano una determinata condizione. Ad esempio, se volessimo eliminare tutte le vocali dalla stringa "Ciao!", il codice sarebbe il seguente:

```Haskell
eliminaVocali :: String -> String
eliminaVocali stringa = filter (/= 'a') (filter (/= 'e') (filter (/= 'i') (filter (/= 'o') (filter (/= 'u') stringa))))
```

Il risultato sarebbe `"C!"`, in quanto la funzione `filter` elimina tutti i caratteri che non sono le vocali "a", "e", "i", "o" o "u".

## Approfondimento

Nell'esempio precedente, abbiamo utilizzato `filter` in modo annidato per eliminare più caratteri. Tuttavia, esiste una soluzione più elegante utilizzando la funzione `any`, che permette di verificare se un elemento della lista soddisfa una determinata condizione. Il codice sarebbe il seguente:

```Haskell
eliminaVocali :: String -> String
eliminaVocali stringa = filter (not .(`any` "aeiou"))
```

La funzione `not` serve a invertire il risultato dell'espressione booleana ottenuta da `any`. Inoltre, abbiamo utilizzato una stringa come parametro per `any`, cosa che ci permette di controllare la presenza di più caratteri senza dover aggiungere più funzioni `filter` annidate.

## Vedi anche

- [Funzioni di ordine superiore in Haskell](https://www.senicar.net/156/funzioni-altezza-ordinamento-haskell/)
- [Funzioni di stringa in Haskell](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-String.html)