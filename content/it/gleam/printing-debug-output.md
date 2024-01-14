---
title:    "Gleam: Stampare l'output di debug"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

#Perché

Stampare l'output di debug è una pratica comune durante la programmazione, in quanto consente agli sviluppatori di identificare e risolvere facilmente eventuali errori nel codice. Questo articolo vi guiderà attraverso il processo di stampa dell'output di debug utilizzando il linguaggio di programmazione Gleam.

#Come fare

Per stampare l'output di debug in Gleam, è possibile utilizzare la funzione `io.debug` seguita da un'espressione o una variabile da controllare. Ad esempio:

```Gleam
io.debug("Contenuto della variabile:", variabile)
```

L'output di debug verrà visualizzato nella console quando si esegue il codice.

È possibile utilizzare anche espressioni multiple all'interno della funzione `io.debug` per stampare più variabili o valori. Ad esempio:

```Gleam
io.debug("X =", x, "Y =", y)
```

#Deep Dive

La funzione `io.debug` accetta anche un parametro opzionale `show_values` che consente di visualizzare il valore effettivo di una variabile invece che solo il suo nome. Questo è particolarmente utile quando si stampa l'output di oggetti complessi o elenchi. Ad esempio:

```Gleam
io.debug("Oggetto:", oggetto, show_values = true)
```

Inoltre, è possibile utilizzare la funzione `io.debugf` per includere formattazione nei messaggi di debug. Ad esempio:

```Gleam
io.debugf("Numero di elementi nell'elenco: {}", lunghezza(elenco))
```

#Vedi anche

- Documentazione ufficiale di Gleam su stampa di debug: https://gleam.run/articles/debugging/
- Esempi di codice di stampa di debug in Gleam: https://github.com/yourusername/gleam-debug-examples