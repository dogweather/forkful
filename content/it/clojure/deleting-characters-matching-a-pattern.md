---
title:    "Clojure: Eliminare caratteri corrispondenti a un modello"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Molte volte, quando si lavora con testi o stringhe in Clojure, potrebbe essere necessario rimuovere specifici caratteri che corrispondono ad un certo schema. Ciò può essere utile per pulire i dati o per ricercare informazioni precise. In quest'articolo, esploreremo il processo di eliminazione dei caratteri che corrispondono ad un determinato pattern utilizzando Clojure.

## Come Fare

Per eliminare i caratteri che corrispondono ad un certo pattern, utilizzeremo la funzione `remove` di Clojure. Questa funzione prende in input una sequenza e ritorna una nuova sequenza senza gli elementi che rispettano il dato pattern. Vediamo un esempio pratico:

```Clojure
(remove #{"a" "e" "i" "o" "u"} "ciao") ; output: "c"
```

In questo esempio, stiamo passando una stringa come secondo argomento alla funzione `remove`. La prima stringa passata come parametro è un set contenente le vocali. La funzione `remove` restituirà una nuova stringa senza le vocali, quindi l'output sarà "c".

## Approfondimento

La funzione `remove` è molto utile quando si lavora con sequenze di caratteri. Tuttavia, è importante notare che la funzione non modifica la sequenza originale, ma invece ne crea una nuova. Inoltre, il pattern utilizzato può essere qualsiasi tipo di funzione predicato Clojure, non solo un set come nell'esempio precedente.

Un altro aspetto interessante della funzione `remove` è che non altera solo le stringhe, ma qualsiasi tipo di sequenza. Questo significa che è possibile utilizzarla anche con liste, vettori o mappe.

## Vedi Anche

- Funzione `remove` della documentazione ufficiale di Clojure: https://clojuredocs.org/clojure.core/remove
- Esempi di utilizzo della funzione `remove` su Clojure REPL: https://clojuredocs.org/clojure.core/remove/examples