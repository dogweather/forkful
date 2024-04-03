---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:12.327850-07:00
description: 'Come fare: #.'
lastmod: '2024-03-13T22:44:43.052937-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: Ottenere la data corrente
weight: 29
---

## Come fare:


### Utilizzando l'Interop con Java
L'interoperabilità senza soluzione di continuità di Clojure con Java ti consente di attingere direttamente all'API Java Date-Time. Ecco come puoi ottenere la data corrente:

```clojure
(import java.time.LocalDate)

(defn get-current-date []
  (str (LocalDate/now)))

;; Esempio di output
(get-current-date) ; "2023-04-15"
```

### Utilizzando la libreria clj-time
Per una soluzione più idiomatica in Clojure, potresti optare per la libreria `clj-time`, un wrapper attorno a Joda-Time, anche se per la maggior parte dei nuovi progetti, si raccomanda l'API Java 8 Date-Time integrata. Tuttavia, se preferisci o richiedi `clj-time`:

Prima, aggiungi `clj-time` alle dipendenze del tuo progetto. Nel tuo `project.clj`, include:

```clojure
[clj-time "0.15.2"]
```

Poi, usala per ottenere la data corrente:

```clojure
(require '[clj-time.core :as time])

(defn get-current-date-clj-time []
  (str (time/now)))

;; Esempio di output
(get-current-date-clj-time) ; "2023-04-15T12:34:56.789Z"
```

Entrambi i metodi forniscono modi rapidi ed efficaci per ottenere la data corrente in Clojure, sfruttando la potenza della piattaforma Java sottostante o la comodità di una libreria specifica per Clojure.
