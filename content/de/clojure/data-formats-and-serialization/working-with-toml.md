---
date: 2024-01-26 04:20:20.001987-07:00
description: "Wie geht das: Um mit TOML in Clojure zu arbeiten, ben\xF6tigen Sie eine\
  \ Bibliothek wie `clj-toml`. F\xFCgen Sie diese zuerst Ihrer `deps.edn` hinzu."
lastmod: '2024-03-13T22:44:53.443641-06:00'
model: gpt-4-0125-preview
summary: "Um mit TOML in Clojure zu arbeiten, ben\xF6tigen Sie eine Bibliothek wie\
  \ `clj-toml`."
title: Arbeiten mit TOML
weight: 39
---

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
