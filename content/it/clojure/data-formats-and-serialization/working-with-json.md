---
title:                "Lavorare con JSON"
aliases:
- /it/clojure/working-with-json/
date:                  2024-02-03T19:22:09.763207-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?
Lavorare con il JSON (JavaScript Object Notation) in Clojure comporta l'analisi delle stringhe JSON in strutture dati di Clojure (mappe, vettori) e viceversa. Questo compito è fondamentale per i servizi web, le API e le applicazioni che necessitano di comunicare dati in un formato strutturato e basato su testo perché il JSON è universalmente riconosciuto e supportato in diversi ambienti di programmazione.

## Come fare:
Clojure non include funzioni integrate per lavorare con il JSON, quindi di solito si utilizzano librerie di terze parti. `cheshire` e `jsonista` sono scelte popolari grazie alla loro facilità d'uso e performance.

### Usare Cheshire
Prima, aggiungi Cheshire alle dipendenze del tuo progetto in `project.clj`:
```clj
[com.fasterxml.jackson.core/jackson-core "2.12.0"]
[cheshire "5.10.1"]
```

Per analizzare una stringa JSON in una mappa di Clojure e convertire una mappa in una stringa JSON:

```clj
(require '[cheshire.core :as json])

;; Analizzare stringa JSON a mappa Clojure
(let [json-input "{\"name\":\"John\", \"age\":30}"]
  (json/parse-string json-input true)) ; => {"name" "John", "age" 30}

;; Convertire mappa Clojure in stringa JSON
(let [clj-map {"name" "John", "age" 30}]
  (json/generate-string clj-map)) ; => "{\"name\":\"John\",\"age\":30}"
```

### Usare Jsonista
Aggiungi Jsonista al tuo progetto `project.clj`:
```clj
[jsonista "0.3.2"]
```

Operazioni simili con Jsonista:

```clj
(require '[jsonista.core :as j])

;; Analizzare stringa JSON a Clojure
(let [json-input "{\"name\":\"Emily\", \"age\":25}"]
  (j/read-value json-input)) ; => {"name" "Emily", "age" 25}

;; Convertire mappa Clojure in stringa JSON
(let [clj-map {"name" "Emily", "age" 25}]
  (j/write-value-as-string clj-map)) ; => "{\"name\":\"Emily\",\"age\":25}"
```

In entrambe le librerie, hai l'opzione di codificare e decodificare strutture dati più complesse, e ci sono funzioni e parametri aggiuntivi che permettono una personalizzazione dei processi di serializzazione e deserializzazione. Per la maggior parte delle applicazioni, la funzionalità dimostrata offre una solida base per lavorare con il JSON in applicazioni Clojure.
