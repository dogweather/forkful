---
title:                "Arbeiten mit TOML"
aliases: - /de/clojure/working-with-toml.md
date:                  2024-01-26T04:20:20.001987-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/working-with-toml.md"
---

{{< edit_this_page >}}

## Was & Warum?
Mit TOML zu arbeiten bedeutet, dass man Daten in einem Minimalformat "Tom's Obvious, Minimal Language" handhabt, das für Konfigurationsdateien wegen seiner leichten Lesbarkeit beliebt ist. Programmierer verwenden es für eine unkomplizierte Konfigurationsverwaltung, die direkt aus dem Kasten heraus mit einer menschenfreundlichen Syntax funktioniert.

## Wie geht das:
Um mit TOML in Clojure zu arbeiten, benötigen Sie eine Bibliothek wie `clj-toml`. Fügen Sie diese zuerst Ihrer `deps.edn` hinzu:

```clojure
{:deps {clj-toml {:mvn/version "0.5.0"}}}
```

Dann parsen Sie etwas TOML:

```clojure
(require '[clj-toml.core :as toml])

(def config-str "title = 'TOML Example'")

(def parsed-config (toml/parse-string config-str))

;; Den Titel aus dem geparsten TOML holen
(println (:title parsed-config)) ;; Ausgabe: TOML Beispiel
```

Um TOML zu generieren:

```clojure
(def data {:title "TOML Beispiel"})

(println (toml/generate-string data))
;; Ausgabe: title = "TOML Beispiel"
```

## Tiefergehend
TOML wurde etwa 2013 von Tom Preston-Werner, dem Mitbegründer von GitHub, als einfachere Alternative zu YAML und JSON für Konfigurationsdateien geschaffen. Es zielt auf Klarheit ab und beabsichtigt, eine Spezifikation zu sein, die Menschen ohne zusätzliche Werkzeuge lesen können.

Während JSON oft für APIs und Web-Apps verwendet wird und YAML mit Referenzen und Skriptfähigkeiten komplex werden kann, sticht TOML mit einem Fokus auf einfache, tabellenbasierte Strukturen hervor. Diese Einfachheit macht es besonders in der Rust-Community und anderen modernen Sprachumgebungen beliebt.

Clojure, mit seinem Fokus auf Einfachheit und Praxistauglichkeit, passt gut zu TOML für die Konfiguration. `clj-toml` oder alternative Bibliotheken schließen die Lücke. Sie übersetzen die statischen Daten von TOML in die dynamische, funktionale Welt von Clojure.

## Siehe auch
- TOMLs GitHub-Repo: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- `clj-toml` auf Clojars: [clojars.org/clj-toml](https://clojars.org/clj-toml)
- Clojure-Dokumentation: [clojure.org](https://clojure.org/guides/getting_started)
- Einführung in `clj-toml`: [github.com/lantiga/clj-toml](https://github.com/lantiga/clj-toml)
