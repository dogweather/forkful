---
title:    "Clojure: Stampa dell'output di debug"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perché

Stampare l'output di debug è un'attività importante per i programmatori che vogliono monitorare il comportamento del loro codice e individuare eventuali errori. Grazie alla stampa di output di debugging, è possibile osservare il flusso del programma e verificare che i dati siano corretti.

## Come farlo

Per stampare l'output di debugging in Clojure, è possibile utilizzare la funzione `println` seguita dalle variabili o espressioni che si vuole visualizzare. Ad esempio:

```Clojure
(def x 5)
(def y 10)
(println "Il valore di x è " x)
(println "Il valore di y è " y)
```

L'esempio sopra stamperebbe l'output seguente:

```
Il valore di x è 5 
Il valore di y è 10
```

Una volta che l'output di debugging è stato stampato, è necessario analizzarlo attentamente per identificare eventuali errori o anomalie che potrebbero causare problemi nel programma.

## Approfondimento

Per una maggiore flessibilità e controllo sull'output di debugging, è possibile utilizzare la macro `prn`, che stampa i dati con un formato più leggibile per il programmatore. Inoltre, è possibile utilizzare la funzione `str` per concatenare le stringhe e visualizzare più dati in un unico output.

Oltre alla stampa di output di debugging, è anche possibile utilizzare un debugger visuale come CIDER o nREPL per esaminare il codice e i dati in tempo reale durante l'esecuzione.

## Vedi anche

- [Clojure Debugging Tips](http://blog.cognitect.com/blog/2016/12/5/clojure-debugging-tips)
- [CIDER debugging guide](https://cider.readthedocs.io/en/latest/debugging/)
- [nREPL debugging guide](https://nrepl.org/nrepl/usage/debug-integration.html)