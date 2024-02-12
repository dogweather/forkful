---
title:                "Praca z TOML"
aliases:
- pl/clojure/working-with-toml.md
date:                  2024-01-26T04:20:37.799581-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/working-with-toml.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Praca z TOML oznacza obsługę danych w formacie Minimal "Tom's Obvious, Minimal Language", popularnym w przypadku plików konfiguracyjnych ze względu na łatwość odczytu. Programiści używają go do prostego zarządzania konfiguracją, która działa od razu z przyjazną dla człowieka składnią.

## Jak to zrobić:
Aby pracować z TOML w Clojure, potrzebujesz biblioteki takiej jak `clj-toml`. Najpierw dodaj ją do swojego `deps.edn`:

```clojure
{:deps {clj-toml {:mvn/version "0.5.0"}}}
```

Następnie przetłumacz jakiś TOML:

```clojure
(require '[clj-toml.core :as toml])

(def config-str "title = 'TOML Example'")

(def parsed-config (toml/parse-string config-str))

;; Pobierz tytuł z przetłumaczonego TOML
(println (:title parsed-config)) ;; Wyjście: TOML Example
```

Aby wygenerować TOML:

```clojure
(def data {:title "TOML Example"})

(println (toml/generate-string data))
;; Wyjście: title = "TOML Example"
```

## Zagłębiając się
TOML został stworzony około 2013 roku przez Toma Preston-Wernera, współzałożyciela GitHuba, jako prostsza alternatywa dla YAML i JSON dla plików konfiguracyjnych. Ma na celu jasność i zamierza być specyfikacją, którą ludzie mogą czytać bez dodatkowych narzędzi.

Podczas gdy JSON jest często używany dla API i aplikacji internetowych, a YAML może stać się skomplikowany z powodu odniesień i możliwości skryptowych, TOML wyróżnia się koncentracją na prostych, tabelarycznych strukturach. Ta prostota sprawia, że jest szczególnie popularny w społeczności Rust i innych nowoczesnych środowiskach językowych.

Clojure, ze swoim skupieniem na prostocie i praktyczności, dobrze współpracuje z TOML dla konfiguracji. `clj-toml` lub alternatywne biblioteki wypełniają lukę. Przekładają statyczne dane TOML na dynamiczny, funkcjonalny świat Clojure.

## Zobacz również
- Repozytorium TOML na GitHubie: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- `clj-toml` na Clojars: [clojars.org/clj-toml](https://clojars.org/clj-toml)
- Dokumentacja Clojure: [clojure.org](https://clojure.org/guides/getting_started)
- Wprowadzenie do `clj-toml`: [github.com/lantiga/clj-toml](https://github.com/lantiga/clj-toml)
