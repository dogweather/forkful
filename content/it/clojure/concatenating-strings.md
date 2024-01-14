---
title:                "Clojure: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché
Stringhe concatenate sono utili quando si vuole creare una nuova stringa che contiene parti di stringhe diverse. Questo è particolarmente utile quando si lavora con testo o dati dinamici che devono essere combinati in una singola stringa.

## Come Fare
Per concatenare stringhe in Clojure, è possibile utilizzare la funzione `str`. Questa funzione prende in input una serie di valori e li concatena in una sola stringa. Ecco un esempio di come utilizzarla:

```Clojure 
(str "Ciao" " " "Mondo") 

Output: "Ciao Mondo"
```

Si può anche utilizzare la funzione `format`, che è utile quando si vuole formattare la stringa in un modo specifico. Ecco un esempio:

```Clojure
(format "Ciao %s!" "Mondo") 

Output: "Ciao Mondo!"
```

Inoltre, se si vuole concatenare più di due stringhe, si può utilizzare la funzione `clojure.string/join`. Questa funzione prende in input una sequenza di stringhe e le concatena utilizzando un separatore specificato. Ecco un esempio:

```Clojure
(clojure.string/join " " ["Ciao" "Mondo" "!"]) 

Output: "Ciao Mondo !"
```

## Approfondimento
La funzione `str` è in realtà molto potente e permette di concatenare non solo stringhe, ma anche numeri e altri tipi di dati. Inoltre, è possibile utilizzare la formattazione di stringhe basata su templating, simile a quella utilizzata in Python. Per esempio:

```Clojure
(str "Il risultato è %s" 5) 

Output: "Il risultato è 5"
```

È importante notare che se si utilizzano molte concatenazioni di stringhe o si lavora con grandi quantità di dati, è consigliabile utilizzare la libreria `clojure.string` invece della funzione `str`, in quanto è più efficiente.

## Vedi Anche
- [Documentazione ufficiale di Clojure sulle stringhe concatenate](https://clojure.org/reference/strings)
- [Guida su come formattare le stringhe in Clojure](https://clojuredocs.org/clojure.string/format)