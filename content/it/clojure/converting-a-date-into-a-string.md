---
title:                "Convertire una data in una stringa"
html_title:           "Clojure: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Convertire una data in una stringa è il processo di trasformare una data, rappresentata come uno specifico formato di dati, in una rappresentazione più leggibile e comprensibile per gli umani. I programmatori spesso eseguono questa conversione per ottenere un output più user-friendly o per adattare i dati a un formato specifico richiesto da un'interfaccia utente.

## Come fare:
```Clojure
(require '[clojure.java-time :as t])

;; ottenere la data corrente
(t/local-date)

;; convertire in stringa con formato "dd/mm/yyyy"
(t/format (t/local-date) "dd/MM/yyyy")
;; output => "28/07/2021"

;; convertire in stringa con formato personalizzato: "MM/yy"
(t/format (t/local-date) "MM/yy")
;; output => "07/21"
```

## Approfondimento:
La conversione delle date è un'attività comune in programmazione, poiché le date vengono spesso utilizzate per tenere traccia delle informazioni temporali. In passato, la maggior parte dei linguaggi di programmazione ha utilizzato librerie esterne per manipolare le date, ma con l'introduzione di Clojure, questa funzionalità è integrata nel linguaggio stesso attraverso la libreria `java-time`.

In alternativa, i programmatori possono utilizzare librerie di terze parti come `clj-time`, che offre funzionalità aggiuntive per la manipolazione delle date in Clojure.

Per quanto riguarda l'implementazione, la libreria `java-time` si basa sulle classi date/ ora di Java, come `LocalDate`, `LocalDateTime`, ecc. per fornire funzionalità di manipolazione delle date. Queste classi forniscono anche metodi per formattare le date in stringhe utilizzando un'ampia gamma di formati predefiniti o personalizzati.

## Vedi anche:
- Documentazione ufficiale di Clojure `java-time` (https://clojure.github.io/java.time-api/)
- Libreria `clj-time` (https://github.com/clj-time/clj-time)