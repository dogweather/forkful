---
title:                "Arbeid med yaml"
html_title:           "Clojure: Arbeid med yaml"
simple_title:         "Arbeid med yaml"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Working with YAML is a way to easily store and transfer data in a structured format. Programmers often use YAML for configuration files or data serialization between different systems.

# Hvordan:
Koding eksempler og eksempeloutput innenfor ```Clojure...``` kodeblokker.

```Clojure
;; Definere en map:
(def test {:navn "Alice"
           :alder 25
           :hobbyer ["fotografering" "reise" "matlaging"]})

;; Skrive til YAML fil:
(require '[clojure.data.yaml :as yaml])
(binding [yaml/*dump-tags* false]
  (yaml/spit "test.yaml" test))

;; Les fra YAML fil:
(yaml/read-str "test.yaml")

;; Output:
{:navn "Alice"
 :alder 25
 :hobbyer ["fotografering" "reise" "matlaging"]}
```

# Dypt dykk:
YAML ble utviklet i 2001 og står for "YAML Ain't Markup Language". Det er et enkelt og leselig dataformat, basert på avfestede datamaskinrepresentasjoner av typet objekter og lister. Alternativer til YAML inkluderer JSON og XML. I Clojure brukes YAML for å representere java objekter og datastrukturer i en leselig format.

# Se også:
- [Offisielt YAML nettsted](https://yaml.org/)
- [YAML syntax guide](https://learnxinyminutes.com/docs/yaml/)