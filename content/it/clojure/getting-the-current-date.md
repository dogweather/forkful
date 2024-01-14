---
title:    "Clojure: Ottenere la data corrente"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Perché
Ci sono molte ragioni per cui potresti essere interessato a ottenere la data corrente nel tuo programma Clojure. Potresti voler mostrare la data all'utente finale, utilizzarla per eseguire alcune operazioni specifiche o semplicemente per tenere traccia di quando è stato eseguito il tuo codice.

## Come Fare
Per ottenere la data corrente in Clojure, puoi utilizzare la funzione `now` del namespace "java.time" che è stata introdotta nella versione 1.8 di Java. Esempio di codice:

```Clojure
(require '[java.time :as time])
(time/now)
```

L'output di questo codice sarà una istanza del tipo `Instant` che rappresenta un istante specifico nel tempo.

Puoi anche formattare la data corrente in un formato specifico utilizzando la funzione `format` del namespace "java.time.format". Esempio di codice:

```Clojure
(require '[java.time :as time])
(require '[java.time.format :as format])
(def formatter (format.DateTimeFormatter/ofPattern "dd/MM/yyyy"))
(time/now)
```

L'output di questo codice sarà la data corrente nel formato specificato, ad esempio "09/05/2021".

## Deep Dive
Se vuoi saperne di più sull'oggetto `Instant` che rappresenta la data corrente, puoi esplorare le sue proprietà e metodi utilizzando la funzione `dir` di Clojure. Esempio di codice:

```Clojure
(require '[java.time :as time])
(def now (time/now))
(dir now)
```

Questo ti mostrerà tutte le proprietà e i metodi disponibili per l'oggetto `Instant`, come ad esempio `getEpochSecond` per ottenere il numero di secondi trascorsi dalla mezzanotte del 1 gennaio 1970.

## Vedi Anche
- [Documentazione ufficiale di Clojure sull'utilizzo del namespace "java.time"](https://clojure.github.io/java-time/)
- [Tutorial su come ottenere la data corrente in Clojure](https://www.baeldung.com/clojure-java-time)