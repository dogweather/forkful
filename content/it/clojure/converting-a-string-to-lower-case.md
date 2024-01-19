---
title:                "Convertire una stringa in minuscolo"
html_title:           "Arduino: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Cosa & Perchè?
Convertire una stringa in minuscolo significa trasformare tutti i caratteri maiuscoli in minuscoli. I programmatori lo fanno per renderla uniforme: è utile per confronti di stringhe, ricerca e ordinamento.

## Come fare:
In Clojure, è semplice utilizzare il metodo `lower-case`.
```Clojure
; definire una stringa
(def mia-stringa "Ciao Mondo")

; convertire la stringa in minuscolo
(println (clojure.string/lower-case mia-stringa))
```
Ecco il risultato:
```Clojure
"ciao mondo"
```

## Immersione profonda:
La funzionalità di conversione di una stringa in minuscolo è presente nella maggior parte dei linguaggi di programmazione dal loro esordio. È utile per prevenire errori dovuti a discrepanze nelle lettere maiuscole o minuscole. In Clojure, la funzione `lower-case` fa parte del modulo `clojure.string` incluso dal Clojure 1.2.

Un'alternativa, anche se più macchinosa, è implementare manualmente questa funzionalità tramite un ciclo sui caratteri della stringa, convertendo uno alla volta.

Per quanto riguarda i dettagli di implementazione, la funzione `lower-case` di Clojure sfrutta il metodo `toLowerCase` della classe String di Java. 

## Vedere anche:
1. Documentazione di Clojure sulle stringhe: https://clojuredocs.org/clojure.string/lower-case
2. Confronto delle funzioni di stringa in Clojure: https://clojure.org/guides/comparators
3. Metodo `toLowerCase` di Java: https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toLowerCase()