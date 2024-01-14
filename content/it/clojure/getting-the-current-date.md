---
title:                "Clojure: Ottenere la data attuale"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

La funzione `java.time.LocalDate/now` è uno strumento essenziale per ottenere la data corrente nel tuo programma Clojure. Con questa funzione, puoi facilmente aggiornare i tuoi dati e fare calcoli basati sulle date attuali.

## Come Fare

```Clojure 
(import 'java.time.LocalDate)

;; Ottenere la data corrente
(def data-oggi (LocalDate/now))

;; Convertire in stringa
(def stringa-data (str data-oggi))

;; Utilizzare format 
;; per specificare il formato della data da stampare
(def data-formato (LocalDate/now))
(def formato (java.time.format.DateTimeFormatter/ofPattern "dd/MM/yyyy"))

(formato data-formato)
```

L'esempio qui sopra ti mostrerà come ottenere la data corrente nel formato desiderato utilizzando `LocalDate/now` e `formato`.

### Output

```
06/08/2021
```

## Approfondimento

Oltre ad utilizzare `LocalDate/now` per ottenere la data corrente, puoi anche utilizzare le funzioni `(.getYear data)`, `(.getMonth data)` e `(.getDayOfMonth data)` per estrarre informazioni specifiche dalla data.

È inoltre possibile manipolare le date utilizzando le funzioni `(.plusDays data giorni)` e `(.minusDays data giorni)` per aggiungere o sottrarre un numero specifico di giorni alla data corrente.

## Vedi Anche

- [Documentazione ufficiale di Clojure sulle date e gli orari](https://clojuredocs.org/clojure.java-time)
- [Clojure Cookbook: Getting the Current Date and Time](https://clojure-cookbook.luminusweb.net/dates-and-times.html)