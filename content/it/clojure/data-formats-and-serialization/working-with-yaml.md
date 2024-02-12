---
title:                "Lavorare con YAML"
aliases:
- /it/clojure/working-with-yaml/
date:                  2024-02-03T19:24:51.560340-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?

YAML, un acronimo ricorsivo per "YAML Ain't Markup Language" (YAML non è un linguaggio di markup), è un formato di serializzazione dati leggibile dall'uomo usato per file di configurazione e scambio di dati tra linguaggi con diverse strutture di dati. I programmatori sfruttano YAML per la sua semplicità e leggibilità, rendendolo una scelta ideale per configurare applicazioni e facilitare lo scambio di dati in ambienti di programmazione poliglotta.

## Come fare:

Clojure non include supporto incorporato per YAML, ma puoi utilizzare librerie di terze parti come `clj-yaml` per analizzare e generare dati YAML. Prima, aggiungi la libreria alle dipendenze del tuo progetto:

```clojure
;; Aggiungi questo alle dipendenze del tuo project.clj
[clj-yaml "0.7.0"]
```

Ecco come puoi usare `clj-yaml` per analizzare YAML e convertire mappe di Clojure in YAML.

### Analisi di YAML:

```clojure
(require '[clj-yaml.core :as yaml])

;; Analisi di una stringa YAML
(let [yaml-str "name: John Doe\nage: 30\nlanguages:\n  - Clojure\n  - Python"]
  (yaml/parse-string yaml-str))
;; Output:
;; => {"name" "John Doe", "age" 30, "languages" ["Clojure" "Python"]}
```

### Generazione di YAML da Clojure:

```clojure
(require '[clj-yaml.core :as yaml])

;; Conversione di una mappa di Clojure in una stringa YAML
(let [data-map {:name "Jane Doe" :age 28 :languages ["Java" "Ruby"]}]
  (yaml/generate-string data-map))
;; Output:
; "age: 28\nlanguages:\n- Java\n- Ruby\nname: Jane Doe\n"
```

Queste semplici operazioni con `clj-yaml` possono essere integrate in applicazioni Clojure per gestire file di configurazione o facilitare lo scambio di dati con altri servizi o componenti che usano YAML.
