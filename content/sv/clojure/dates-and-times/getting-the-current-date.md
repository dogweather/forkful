---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:17.352092-07:00
description: "Hur man g\xF6r: Clojures s\xF6ml\xF6sa interoperabilitet med Java l\xE5\
  ter dig direkt utnyttja Java Date-Time API. S\xE5 h\xE4r kan du f\xE5 det aktuella\
  \ datumet."
lastmod: '2024-03-13T22:44:37.535621-06:00'
model: gpt-4-0125-preview
summary: "Clojures s\xF6ml\xF6sa interoperabilitet med Java l\xE5ter dig direkt utnyttja\
  \ Java Date-Time API."
title: "F\xE5 det aktuella datumet"
weight: 29
---

## Hur man gör:


### Använda Java Interop
Clojures sömlösa interoperabilitet med Java låter dig direkt utnyttja Java Date-Time API. Så här kan du få det aktuella datumet:

```clojure
(import java.time.LocalDate)

(defn get-current-date []
  (str (LocalDate/now)))

;; Exempelutskrift
(get-current-date) ; "2023-04-15"
```

### Använda clj-time-biblioteket
För en mer idiomatisk Clojure-lösning kan du välja `clj-time`-biblioteket, ett omslag runt Joda-Time, även om det för de flesta nya projekt rekommenderas att använda det inbyggda Java 8 Date-Time API:et. Dock, om du föredrar eller behöver `clj-time`:

Först, lägg till `clj-time` i dina projektberoenden. I din `project.clj`, inkludera:

```clojure
[clj-time "0.15.2"]
```

Sedan använder du det för att få det aktuella datumet:

```clojure
(require '[clj-time.core :as time])

(defn get-current-date-clj-time []
  (str (time/now)))

;; Exempelutskrift
(get-current-date-clj-time) ; "2023-04-15T12:34:56.789Z"
```

Båda metoderna ger snabba, effektiva sätt att få det aktuella datumet i Clojure, genom att utnyttja kraften av den underliggande Java-plattformen eller bekvämligheten av ett Clojure-specifikt bibliotek.
