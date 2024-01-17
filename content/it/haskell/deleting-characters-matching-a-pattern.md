---
title:                "Eliminazione di caratteri corrispondenti a un modello"
html_title:           "Haskell: Eliminazione di caratteri corrispondenti a un modello"
simple_title:         "Eliminazione di caratteri corrispondenti a un modello"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Cosa e' e perche': 
La cancellazione dei caratteri corrispondenti ad un determinato schema e' un'operazione comune nel mondo della programmazione. Ci permette di rimuovere determinati caratteri da una stringa o da un testo, in base ad un modello che definiamo. Questo ci aiuta nella manipolazione dei dati e nella creazione di algoritmi efficaci.

## Come fare: 
Per cancellare i caratteri corrispondenti ad un pattern in Haskell, possiamo utilizzare la funzione `filter`, che prende come argomenti una funzione che valuta il pattern e la lista di caratteri da cui eliminare i corrispondenti. Ad esempio, se vogliamo cancellare tutti i numeri da una lista di caratteri, possiamo utilizzare la seguente espressione:

```Haskell 
filter (\x -> not (x `elem` ['0'..'9'])) "abc123def"
```

Il risultato di questa espressione sarebbe "abcdef", poiche' tutti i numeri sono stati cancellati.

## Approfondimento: 
La cancellazione dei caratteri corrispondenti e' stata introdotta nella programmazione funzionale da richiami al modello `grep` in Unix. Ci sono vari modi per implementare questa funzione in Haskell, come utilizzare un'espressione regolare con il pacchetto `regex` o creare una funzione personalizzata che controlli ogni carattere. E' anche possibile utilizzare la funzione `delete` per rimuovere un singolo carattere specifico da una stringa.

## Vedi anche: 
Per ulteriori informazioni sulla cancellazione dei caratteri corrispondenti in Haskell, consiglio di consultare la documentazione ufficiale o di esplorare progetti open source che utilizzano questa funzione. Alcuni esempi potrebbero essere [`text`](https://hackage.haskell.org/package/text/docs/Data-Text.html) e [`stringsearch`](https://hackage.haskell.org/package/stringsearch/docs/Data-String-Search.html).