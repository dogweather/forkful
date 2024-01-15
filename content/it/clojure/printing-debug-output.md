---
title:                "Stampa output di debug"
html_title:           "Clojure: Stampa output di debug"
simple_title:         "Stampa output di debug"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perchè

Impressionare il proprio codice debug è una tattica utile per identificare problemi e risolverli rapidamente. È un modo semplice ma efficace per comprendere il flusso di esecuzione del proprio programma e individuare eventuali errori.

## Come

L'uso più comune di println in Clojure è per stampare il valore di una variabile durante l'esecuzione del codice. Ecco un esempio:

```Clojure
(def num 5)
(println "Il valore di num è:" num)
```

Questo codice stamperà: "Il valore di num è: 5" nell'output della console. Puoi anche utilizzare println per stampare più variabili, come nel seguente esempio:

```Clojure
(def str "ciao")
(def num 10)
(println "La stringa è:" str ", il numero è:" num)
```

Questo stamperà: "La stringa è: ciao, il numero è: 10". Inoltre, puoi utilizzare println anche per stampare il valore di una funzione durante l'esecuzione del codice.

## Deep Dive

Oltre ad essere utile per stampare il valore di variabili e funzioni, println è uno strumento versatile per il debug del codice. Puoi utilizzarlo per stampare informazioni su una specifica parte del codice, come il risultato di un'operazione o il valore di una variabile in un determinato momento.

Puoi anche utilizzare la funzione prn per stampare i valori in modo più leggibile, poiché aggiunge spazi tra gli elementi stampati. Inoltre, puoi combinare println o prn con la funzione str per formattare l'output in modo più chiaro e organizzato.

## See Also

- [Documentazione ufficiale di Clojure](https://clojuredocs.org/)
- [Articoli di programmazione di Clojure su Medium](https://medium.com/tag/clojure-programming)
- [Clojure subreddit](https://www.reddit.com/r/Clojure/)