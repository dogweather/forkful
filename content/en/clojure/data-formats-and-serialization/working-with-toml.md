---
aliases:
- /en/clojure/working-with-toml/
date: 2024-01-25 03:39:30.881912-07:00
description: "Working with TOML means you're handling data in a Minimal \"Tom's Obvious,\
  \ Minimal Language\" format, popular for config files due to its easy readability.\u2026"
lastmod: 2024-02-18 23:09:10.746891
model: gpt-4-1106-preview
summary: "Working with TOML means you're handling data in a Minimal \"Tom's Obvious,\
  \ Minimal Language\" format, popular for config files due to its easy readability.\u2026"
title: Working with TOML
---

{{< edit_this_page >}}

## What & Why?
Working with TOML means you're handling data in a Minimal "Tom's Obvious, Minimal Language" format, popular for config files due to its easy readability. Programmers use it for straightforward configuration management that works right out of the box with human-friendly syntax.

## How to:
To work with TOML in Clojure, you need a library like `clj-toml`. First, add it to your `deps.edn`:

```clojure
{:deps {clj-toml {:mvn/version "0.5.0"}}}
```

Then parse some TOML:

```clojure
(require '[clj-toml.core :as toml])

(def config-str "title = 'TOML Example'")

(def parsed-config (toml/parse-string config-str))

;; Get the title from the parsed TOML
(println (:title parsed-config)) ;; Output: TOML Example
```

To generate TOML:

```clojure
(def data {:title "TOML Example"})

(println (toml/generate-string data))
;; Output: title = "TOML Example"
```

## Deep Dive
TOML was created around 2013 by Tom Preston-Werner, co-founder of GitHub, as a simpler alternative to YAML and JSON for config files. It aims for clarity and intends to be a spec humans can read without additional tools. 

While JSON is often used for APIs and web apps, and YAML can get complex with references and script abilities, TOML stands out with a focus on simple, table-based structures. This simplicity makes it especially popular in the Rust community and other modern language environments.

Clojure, with its focus on simplicity and practicality, pairs well with TOML for config. `clj-toml` or alternative libraries bridge the gap. They translate TOML's static data into Clojure's dynamic, functional world.

## See Also
- TOML's GitHub Repo: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- `clj-toml` on Clojars: [clojars.org/clj-toml](https://clojars.org/clj-toml)
- Clojure Docs: [clojure.org](https://clojure.org/guides/getting_started)
- Intro to `clj-toml`: [github.com/lantiga/clj-toml](https://github.com/lantiga/clj-toml)
