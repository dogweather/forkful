---
title:                "Convertire una stringa in minuscolo"
html_title:           "Clojure: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

In molti casi, è necessario convertire una stringa in minuscolo per scopi di formattazione o confronto. Con Clojure, questo è un'operazione molto semplice e veloce grazie alla sua natura funzionale.

## Come Fare

Per convertire una stringa in minuscolo in Clojure, possiamo utilizzare la funzione `lower-case` dal namespace `clojure.string`. Questa funzione accetta una stringa come argomento e restituisce una nuova stringa in lettere minuscole. 

```Clojure
(require '[clojure.string :as str])

(str/lower-case "CIAO MONDO") ; Output: "ciao mondo"
```

Come possiamo vedere dall'esempio sopra, abbiamo utilizzato la funzione `lower-case` aggiungendo il prefisso `str/` che indica che stiamo utilizzando la funzione dal namespace `clojure.string`.

Un'altra opzione è utilizzare la funzione `clojure.core` `lower-case` che accetta una sequenza come argomento e restituisce una nuova sequenza con tutte le stringhe in lettere minuscole. 

```Clojure
(lower-case ["CIAO" "MONDO"]) ; Output: ["ciao" "mondo"]
```

Invece di utilizzare la funzione `clojure.string` `lower-case`, potremmo anche convertire manualmente la stringa in una sequenza di caratteri, applicare la funzione `Character/lowerCase` a ogni carattere e poi unire nuovamente i caratteri in una nuova stringa. Tuttavia, questo metodo richiederebbe molto più codice e non è consigliabile a meno che non sia necessario per scopi specifici. 

## Approfondimento

Mentre il processo per convertire una stringa in minuscolo in Clojure è relativamente semplice, vale la pena notare che questo approccio restituirà una nuova stringa, e non modificherà la stringa originale. Questo è dovuto alla natura immutabile delle stringhe in Clojure.

Inoltre, è importante considerare che la funzione `lower-case` accetta solo una stringa come argomento e non una sequenza di caratteri. Se si desidera convertire una sequenza di caratteri, è necessario utilizzare la funzione `lower-case` dal namespace `clojure.core`, come mostrato negli esempi sopra.

Un altro aspetto da considerare è che la funzione `lower-case` si basa sull'impostazione locale del sistema. Ciò significa che il risultato di `lower-case` può variare a seconda della lingua di sistema e delle impostazioni di localizzazione. Se si desidera assicurarsi che la conversione sia sempre in minuscolo indipendentemente dalle impostazioni di sistema, è possibile utilizzare la funzione `Character/toLowerCase` sulla sequenza di caratteri anziché utilizzare `lower-case`.

## Vedi Anche

Alcune risorse utili per approfondire la conversione di stringhe in minuscolo in Clojure:

- [Documentazione ufficiale di Clojure](https://clojure.org/)
- [Funzione `lower-case` dalla libreria `clojure.string`](https://clojuredocs.org/clojure.string/lower-case)
- [Funzione `lower-case` dalla libreria `clojure.core`](https://clojuredocs.org/clojure.core/lower-case)