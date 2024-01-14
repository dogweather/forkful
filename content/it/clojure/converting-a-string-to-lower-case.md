---
title:    "Clojure: Convertire una stringa in minuscolo"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché
Converting una stringa in lowercase è un'operazione comune e utile quando si lavora con dati di input in formato testuale. Ciò permette di uniformare i dati e semplificare le operazioni di confronto e ricerca.

## Come fare
Per convertire una stringa in lowercase in Clojure, è possibile utilizzare la funzione `clojure.string/lower-case`. Di seguito un esempio di codice con il risultato di output:

```Clojure
(clojure.string/lower-case "CIAO AMICI")
```

Output: `ciao amici`

È importante notare che la funzione `lower-case` può gestire sia stringhe alfanumeriche che caratteri speciali come accenti e simboli.

## Approfondimento
La conversione di una stringa in lowercase può essere influenzata da diversi fattori, come la codifica dei caratteri e le impostazioni regionali del sistema in cui viene eseguito il codice. Inoltre, è possibile utilizzare il metodo `case` per gestire stringhe con più parole, come nel seguente esempio:

```Clojure
(clojure.string/lower-case (case "CIAO AMICI" :lower))
```

Output: `ciao amici`

Questo metodo può essere utile in situazioni in cui si desidera mantenere la prima lettera di ogni parola maiuscola, come nelle intestazioni di un documento.

## Vedi anche
- [Documentation for `clojure.string/lower-case`](https://clojuredocs.org/clojure.string/lower-case)
- [Explanation of `case` in Clojure](https://clojuredocs.org/clojure.core/case)