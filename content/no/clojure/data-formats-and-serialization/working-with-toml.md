---
date: 2024-01-26 04:20:37.681644-07:00
description: "\xC5 jobbe med TOML betyr at du h\xE5ndterer data i et minimalt format\
  \ kalt \"Tom's Obvious, Minimal Language\", som er popul\xE6rt for konfigurasjonsfiler\
  \ p\xE5 grunn\u2026"
lastmod: '2024-03-13T22:44:40.425119-06:00'
model: gpt-4-0125-preview
summary: "\xC5 jobbe med TOML betyr at du h\xE5ndterer data i et minimalt format kalt\
  \ \"Tom's Obvious, Minimal Language\", som er popul\xE6rt for konfigurasjonsfiler\
  \ p\xE5 grunn av dets enkle lesbarhet."
title: Jobbe med TOML
weight: 39
---

## Hva & Hvorfor?
Å jobbe med TOML betyr at du håndterer data i et minimalt format kalt "Tom's Obvious, Minimal Language", som er populært for konfigurasjonsfiler på grunn av dets enkle lesbarhet. Programmerere bruker det til enkel konfigurasjonsstyring som fungerer rett ut av boksen med menneskevennlig syntaks.

## Hvordan:
For å jobbe med TOML i Clojure, trenger du et bibliotek som `clj-toml`. Først legger du det til i din `deps.edn`:

```clojure
{:deps {clj-toml {:mvn/version "0.5.0"}}}
```

Deretter parser du litt TOML:

```clojure
(require '[clj-toml.core :as toml])

(def config-str "title = 'TOML Example'")

(def parsed-config (toml/parse-string config-str))

;; Få tittelen fra den parsete TOML-en
(println (:title parsed-config)) ;; Utdata: TOML Example
```

For å generere TOML:

```clojure
(def data {:title "TOML Example"})

(println (toml/generate-string data))
;; Utdata: title = "TOML Example"
```

## Dypdykk
TOML ble skapt rundt 2013 av Tom Preston-Werner, medgrunnlegger av GitHub, som et enklere alternativ til YAML og JSON for konfigurasjonsfiler. Den sikter mot klarhet og har til hensikt å være en spesifikasjon mennesker kan lese uten ekstra verktøy.

Mens JSON ofte brukes for APIer og webapplikasjoner, og YAML kan bli kompleks med referanser og script-muligheter, skiller TOML seg ut med et fokus på enkle, tabell-baserte strukturer. Denne enkelheten gjør den spesielt populær i Rust-samfunnet og andre moderne språkmiljøer.

Clojure, med sitt fokus på enkelhet og praktiskhet, passer godt sammen med TOML for konfigurasjon. `clj-toml` eller alternative biblioteker brobygger gapet. De oversetter TOMLs statiske data til Clojures dynamiske, funksjonelle verden.

## Se Også
- TOMLs GitHub Repo: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- `clj-toml` på Clojars: [clojars.org/clj-toml](https://clojars.org/clj-toml)
- Clojure Dokumentasjon: [clojure.org](https://clojure.org/guides/getting_started)
- Introduksjon til `clj-toml`: [github.com/lantiga/clj-toml](https://github.com/lantiga/clj-toml)
