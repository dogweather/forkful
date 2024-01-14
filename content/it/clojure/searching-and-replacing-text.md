---
title:    "Clojure: Ricerca e sostituzione di testo"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

Cercare e sostituire il testo è una delle attività più comuni e utili quando si programma in Clojure. Usando questa tecnica, è possibile trovare e sostituire parti del codice in modo veloce e efficiente. In questo post, esploreremo come implementare questa funzionalità utilizzando alcune funzioni di Clojure.

## Come fare

Per cercare e sostituire il testo, useremo le seguenti funzioni di Clojure:

```Clojure
(replace "testoDaCercare" "nuovoTesto" "stringaDaCercare")
(replace-regex #"([0-9]+)" "parola" "1, 2, 3")
```

In questo esempio, la prima funzione sostituisce la prima occorrenza del testo specificato con il nuovo testo nella stringa data. La seconda funzione utilizza una espressione regolare per cercare e sostituire tutte le occorrenze di numeri con la parola "parola". Entrambe le funzioni restituiscono una nuova stringa con il testo sostituito.

## Approfondimento

Oltre alle funzioni di base per cercare e sostituire il testo, Clojure offre anche altre funzioni e librerie utili per questa attività. Ad esempio, la libreria "clojure.string" include la funzione "replace-first" che sostituisce solo la prima occorrenza del testo. È anche possibile utilizzare la funzione "replace-nth" per sostituire solo la n-esima occorrenza.

Inoltre, Clojure ha una caratteristica interessante che consente di sostituire il testo utilizzando una funzione di trasformazione. Questo significa che invece di specificare un nuovo testo, possiamo fornire una funzione che trasforma il testo da sostituire. Ad esempio:

```Clojure
(replace #(str * 2) "3" "2 3 4")
```

In questo esempio, la funzione "replace" sostituisce il testo "3" nella stringa con il doppio del testo stesso, ottenendo come risultato "2 6 4".

## Vedi anche

- [Documentazione ufficiale di Clojure su replace e replace-regex](https://clojuredocs.org/clojure.string/replace)
- [Tutorial su espressioni regolari in Clojure](https://clojuredocs.org/clojure.repl/doc-clojure/regex)
- [Cheat sheet di Clojure per sostituire il testo](https://clojure.org/api/cheatsheet)