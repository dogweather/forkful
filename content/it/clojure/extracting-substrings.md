---
title:    "Clojure: Estrazione di sottostringhe"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

L'estrazione di sottostringhe è un'operazione importante nella programmazione di Clojure perché può essere utilizzata per manipolare e gestire i dati in modo più efficiente. Può anche essere utile per la validazione e la pulizia dei dati.

## Come fare

Ecco un esempio di come estrarre una sottostringa utilizzando Clojure:

```
(s 0 5 "Ciao amici")
```

Questo codice restituirà "Ciao", le prime 5 lettere della stringa "Ciao amici". È importante notare che l'indice inizia da 0 e che la sottostringa viene inclusa nell'output. In questo esempio, abbiamo una stringa "Ciao amici" ma è possibile utilizzare qualsiasi altra stringa al suo posto.

## Approfondimento

L'estrazione di sottostringhe utilizza due argomenti principali: l'indice iniziale e l'indice finale. L'indice iniziale indica da quale carattere iniziare l'estrazione della sottostringa, mentre l'indice finale indica fino a quale carattere deve essere estratto. È possibile utilizzare anche altri argomenti, come l'indice di direzione e la regola di inclusione, per ottenere risultati più precisi.

Inoltre, la funzione di estrazione delle sottostringhe può essere utilizzata anche su altre strutture di dati come vettori, liste e mappe. Ciò la rende una funzione estremamente versatile e utile in molte situazioni.

## Vedi anche

- [Documentazione ufficiale di Clojure sull'estrazione delle sottostringhe] (https://clojuredocs.org/clojure.core/subs)
- [Tutorial di estrazione delle sottostringhe in Clojure] (https://www.braveclojure.com/strings/)
- [Esempi pratici di estrazione di sottostringhe in Clojure] (https://clojure-examples.herokuapp.com/string-substring)

Grazie per aver letto questo articolo e speriamo che ti sia stato utile nell'imparare come estrarre sottostringhe in Clojure. Continua a esplorare questo potente linguaggio di programmazione e scopri tutte le sue funzionalità. Buona programmazione!