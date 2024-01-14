---
title:    "Clojure: Trova la lunghezza di una stringa"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Perché
Solo 1-2 frasi per spiegare *perché* qualcuno dovrebbe impegnarsi a trovare la lunghezza di una stringa. 

A volte nella programmazione, è necessario sapere quanti caratteri ci sono in una stringa. Può essere utile per la validazione dei dati, per il conteggio di parole o per altre operazioni sulle stringhe. Sapere come trovare la lunghezza di una stringa può aiutare a semplificare il processo di codifica e fornire risultati più precisi.

## Come fare
Di seguito sono riportati alcuni esempi di codice in Clojure che mostrano come trovare la lunghezza di una stringa e l'output che ci si può aspettare.

```Clojure
(.length "Ciao!")
;; Output: 5
```
```
(count "Hello world")
;; Output: 11
```

Questi esempi utilizzano rispettivamente il metodo `.length` e la funzione `count` per trovare la lunghezza delle stringhe fornite come argomento.

## Approfondimento
Trovare la lunghezza di una stringa può sembrare un'operazione semplice, ma ci sono alcuni aspetti da tenere in considerazione. Ad esempio, alcuni caratteri speciali o emoji possono essere rappresentati con più di un byte, quindi la loro lunghezza nella stringa potrebbe non corrispondere al numero di caratteri visibili. Inoltre, ci sono metodi per trovare la lunghezza di una stringa che possono essere più efficienti di altri a seconda delle circostanze.

Per maggiori informazioni sull'argomento, si consiglia di esplorare la documentazione ufficiale di Clojure sulle stringhe e di fare qualche esperimento con diversi tipi di stringhe per comprendere meglio come funziona la lunghezza.

## Vedi anche
- Documentazione ufficiale di Clojure sulle stringhe: https://clojuredocs.org/clojure.core/clojure.string
- Tutorial su Clojure per principianti: https://clojure.org/guides/getting_started
- Esempi di esercizi pratici per la lunghezza delle stringhe in Clojure: https://www.codewars.com/kata/search/find%20length%20of%20string/language/Clojure