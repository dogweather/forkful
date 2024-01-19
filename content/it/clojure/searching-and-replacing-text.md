---
title:                "Ricerca e sostituzione del testo"
html_title:           "Arduino: Ricerca e sostituzione del testo"
simple_title:         "Ricerca e sostituzione del testo"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Che Cosa & Perché?

"Ricerca e sostituzione del testo" è una funzione frequente nel codice, utilizzata per trovare corrispondenze di specifici testi e sostituirli con altro. Questo è importante quando vogliamo cambiare i contenuti di una stringa in base a specifiche regole o pattern.

## Come si fa:

In Clojure, puoi utilizzare la funzione `clojure.string/replace` per cercare e sostituire testo. Ecco un semplice esempio:

```Clojure
(require '[clojure.string :as str])

(defn esempio-replace []
  (let [s "Ciao, mondo!"]
   (str/replace s "mondo" "programmatore")))

(esempio-replace)  ; Outputs: "Ciao, programmatore!"
```

In questo esempio, la funzione `esempio-replace` sta cercando la stringa "mondo" nella stringa `s` e la sostituisce con "programmatore".

## Approfondimenti

Storicamente, la ricerca e la sostituzione del testo sono state una parte fondamentale del mondo del editing di testo e della programmazione fin dai primi giorni degli editor di testo.

Alternativamente, è possibile usare espressioni regolari per pattern più complessi di ricerca e sostituzione. In Clojure, è possibile farlo utilizzando la stessa funzione `clojure.string/replace`, ma fornendo un' espressione regolare al posto di una semplice stringa:

```Clojure
(str/replace "Ciao, mondo!" #"\w+" "programmatore")  ; Outputs: "programmatore, programmatore!"
```

Infine, riguardo ai dettagli di implementazione, la funzione `clojure.string/replace` semplicemente chiama il metodo `.replaceAll` di Java sulle stringhe dato che Clojure è costruito sopra la JVM (Java Virtual Machine).

## Ulteriori Riferimenti

Per saperne di più sulla manipolazione delle stringhe in Clojure, dai un'occhiata a questi link:

- La documentazione ufficiale del modulo Clojure string: [https://clojuredocs.org/clojure.string](https://clojuredocs.org/clojure.string) 
- Una guida sull'utilizzo delle espressioni regolari in Clojure: [https://www.regular-expressions.info/clojure.html](https://www.regular-expressions.info/clojure.html)
- Una spiegazione più dettagliata sulla funzione `replace` di Clojure: [https://clojuredocs.org/clojure.string/replace](https://clojuredocs.org/clojure.string/replace)