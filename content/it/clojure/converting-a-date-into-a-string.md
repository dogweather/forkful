---
title:    "Clojure: Convertire una data in una stringa"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

Convertire una data in una stringa è un'operazione comune quando si lavora con dati temporali in un programma Clojure. In questo articolo, esploreremo perché è importante saperlo fare e come farlo nel modo corretto.

## Come fare

Per convertire una data in una stringa usando Clojure, puoi utilizzare la funzione `format` della libreria `clojure.tools.logging`. Nell'esempio seguente, stiamo convertendo la data del 1° gennaio 2020 in una stringa nel formato "gg/MM/aaaa":

```Clojure
(require '[clojure.tools.logging :as log])

(def data (java.util.Date. 120, 0, 1)) 
(def stringa-data (log/format data "dd/MM/yyyy")) 

(println stringa-data) 
```

Output: "01/01/2020"

La funzione `format` prende due argomenti: la data da convertire e una stringa di formato. Puoi utilizzare diversi simboli di formato per creare la tua stringa di formato, come ad esempio "dd" per il giorno, "MM" per il mese e "yyyy" per l'anno.

## Approfondimenti

Se vuoi approfondire ulteriormente l'argomento della conversione di date in stringhe in Clojure, puoi esplorare le diverse opzioni di formattazione disponibili nella documentazione della funzione `format`. Inoltre, puoi anche imparare come gestire fusioni di fusi orari e altri problemi comuni che possono verificarsi quando si lavora con date in un ambiente distribuito.

## Vedi anche

- [Documentazione della funzione `format`](https://clojure.github.io/tools.logging/#clojure.tools.logging/format)
- [Tutorial su come lavorare con date in Clojure](https://www.baeldung.com/clojure-dates)
- [Esempi di manipolazione delle date in Clojure](https://peshmerge.io/learn-clojure/dates/)