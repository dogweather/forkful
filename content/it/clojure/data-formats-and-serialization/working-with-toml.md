---
date: 2024-01-26 04:20:24.429281-07:00
description: "Lavorare con TOML significa gestire i dati in un formato minimale \"\
  Tom's Obvious, Minimal Language\", popolare per i file di configurazione grazie\
  \ alla sua\u2026"
lastmod: '2024-03-13T22:44:43.065859-06:00'
model: gpt-4-0125-preview
summary: "Lavorare con TOML significa gestire i dati in un formato minimale \"Tom's\
  \ Obvious, Minimal Language\", popolare per i file di configurazione grazie alla\
  \ sua\u2026"
title: Lavorare con TOML
---

{{< edit_this_page >}}

## Cosa & Perché?
Lavorare con TOML significa gestire i dati in un formato minimale "Tom's Obvious, Minimal Language", popolare per i file di configurazione grazie alla sua facile leggibilità. I programmatori lo utilizzano per una gestione della configurazione semplice che funziona subito con una sintassi amichevole per l'utente.

## Come fare:
Per lavorare con TOML in Clojure, hai bisogno di una libreria come `clj-toml`. Prima, aggiungila al tuo `deps.edn`:

```clojure
{:deps {clj-toml {:mvn/version "0.5.0"}}}
```

Poi analizza un po' di TOML:

```clojure
(require '[clj-toml.core :as toml])

(def config-str "title = 'Esempio TOML'")

(def parsed-config (toml/parse-string config-str))

;; Ottieni il titolo dal TOML analizzato
(println (:title parsed-config)) ;; Output: Esempio TOML
```

Per generare TOML:

```clojure
(def data {:title "Esempio TOML"})

(println (toml/generate-string data))
;; Output: title = "Esempio TOML"
```

## Approfondimento
TOML è stato creato nel 2013 da Tom Preston-Werner, co-fondatore di GitHub, come un'alternativa più semplice a YAML e JSON per i file di configurazione. Mira alla chiarezza e intende essere uno standard che gli umani possano leggere senza strumenti aggiuntivi.

Mentre JSON è spesso utilizzato per le API e le applicazioni web, e YAML può diventare complesso con riferimenti e capacità di scripting, TOML spicca con un focus su strutture semplici e basate su tabelle. Questa semplicità lo rende particolarmente popolare nella comunità Rust e in altri ambienti di linguaggi moderni.

Clojure, con il suo focus sulla semplicità e praticità, si abbina bene con TOML per le configurazioni. `clj-toml` o librerie alternative colmano il divario. Traducono i dati statici di TOML nel mondo dinamico e funzionale di Clojure.

## Vedi Anche
- Repo GitHub di TOML: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- `clj-toml` su Clojars: [clojars.org/clj-toml](https://clojars.org/clj-toml)
- Documentazione di Clojure: [clojure.org](https://clojure.org/guides/getting_started)
- Introduzione a `clj-toml`: [github.com/lantiga/clj-toml](https://github.com/lantiga/clj-toml)
