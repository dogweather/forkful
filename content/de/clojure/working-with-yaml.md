---
title:                "Arbeiten mit YAML"
html_title:           "Bash: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## Was & Warum?
YAML ist ein Datenformat ähnlich wie JSON, aber leichter zu lesen. Programmierer nutzen YAML zum Konfigurieren von Software und zum Austauschen von Daten, weil es menschenlesbar und einfach zu verstehen ist.

## How to:
Um mit YAML in Clojure zu arbeiten, verwende die Bibliothek `clj-yaml`. Hier ist ein Beispiel, wie du eine YAML-Datei einliest und sie in Clojure-Datenstrukturen umwandelst. Installiere `clj-yaml` über Leiningen mit `[clj-yaml "0.7.0"]` oder mit Clojure CLI/deps.edn `clj-yaml {:mvn/version "0.7.0"}`.

```Clojure
(require '[clj-yaml.core :as yaml])

;; YAML-String in Clojure-Datenstruktur umwandeln
(let [yaml-str "name: Max
age: 30
languages:
  - Clojure
  - Python"]
  (println (yaml/parse-string yaml-str)))
```

Ausgabe:
```Clojure
{:name "Max", :age 30, :languages ["Clojure" "Python"]}
```

Und hier kannst du Clojure-Datenstrukturen in YAML umwandeln:

```Clojure
(def data {:name "Max", :age 30, :languages ["Clojure" "Python"]})

(println (yaml/generate-string data))
```

Ausgabe:
```YAML
name: Max
age: 30
languages:
- "Clojure"
- "Python"
```

## Tiefere Einblicke
YAML steht für "YAML Ain't Markup Language" (oder rekursiv "YAML Ain't Markup Language") und wurde Anfang der 2000er als eine vereinfachte Alternative zu XML entwickelt. Im Gegensatz zu JSON unterstützt YAML Kommentare und kann Relationen durch Details wie Verankerungen und Verweise besser darstellen. Alternativen umfassen JSON und TOML, wobei die Wahl oft von persönlichen Vorlieben oder projektspezifischen Anforderungen abhängt. Clojure-YAML verwendet die SnakeYAML-Bibliothek für das Java-Interoperability-Back-End, was eine starke und zuverlässige Verarbeitung garantiert.

## Siehe Auch
- YAML offizielle Webseite: https://yaml.org
- clj-yaml GitHub Repository: https://github.com/clj-commons/clj-yaml
- SnakeYAML, Java-Bibliothek verwendet von clj-yaml: https://bitbucket.org/asomov/snakeyaml
