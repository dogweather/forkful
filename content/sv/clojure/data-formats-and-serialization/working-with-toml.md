---
date: 2024-01-26 04:20:39.508304-07:00
description: "Hur man g\xF6r: F\xF6r att arbeta med TOML i Clojure beh\xF6ver du ett\
  \ bibliotek som `clj-toml`. L\xE4gg f\xF6rst till det i din `deps.edn`."
lastmod: '2024-03-13T22:44:37.548517-06:00'
model: gpt-4-0125-preview
summary: "F\xF6r att arbeta med TOML i Clojure beh\xF6ver du ett bibliotek som `clj-toml`."
title: Att arbeta med TOML
weight: 39
---

## Hur man gör:
För att arbeta med TOML i Clojure behöver du ett bibliotek som `clj-toml`. Lägg först till det i din `deps.edn`:

```clojure
{:deps {clj-toml {:mvn/version "0.5.0"}}}
```

Sedan tolkar du lite TOML:

```clojure
(require '[clj-toml.core :as toml])

(def config-str "title = 'TOML Example'")

(def parsed-config (toml/parse-string config-str))

;; Hämta titeln från det tolkade TOML
(println (:title parsed-config)) ;; Utdata: TOML Example
```

För att generera TOML:

```clojure
(def data {:title "TOML Example"})

(println (toml/generate-string data))
;; Utdata: title = "TOML Example"
```

## Djupdykning
TOML skapades runt 2013 av Tom Preston-Werner, medgrundare av GitHub, som ett enklare alternativ till YAML och JSON för konfigurationsfiler. Det syftar till klarhet och avser att vara en spec människor kan läsa utan ytterligare verktyg.

Medan JSON ofta används för API:er och webbapplikationer, och YAML kan bli komplicerat med referenser och skriptmöjligheter, sticker TOML ut med fokus på enkla, tabellbaserade strukturer. Denna enkelhet gör det särskilt populärt i Rust-gemenskapen och andra moderna programspråksmiljöer.

Clojure, med fokus på enkelhet och praktisk tillämpning, passar väl ihop med TOML för konfig. `clj-toml` eller alternativa bibliotek överbryggar klyftan. De översätter TOMLs statiska data till Clojures dynamiska, funktionella värld.

## Se även
- TOMLs GitHub Repo: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- `clj-toml` på Clojars: [clojars.org/clj-toml](https://clojars.org/clj-toml)
- Clojure Dokumentation: [clojure.org](https://clojure.org/guides/getting_started)
- Introduktion till `clj-toml`: [github.com/lantiga/clj-toml](https://github.com/lantiga/clj-toml)
