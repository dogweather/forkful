---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:05.596768-07:00
description: "Jak to zrobi\u0107: Clojure nie zawiera wbudowanego wsparcia dla YAML,\
  \ ale mo\u017Cesz u\u017Cy\u0107 bibliotek stron trzecich, takich jak `clj-yaml`,\
  \ do analizowania i\u2026"
lastmod: '2024-03-13T22:44:35.017521-06:00'
model: gpt-4-0125-preview
summary: "Clojure nie zawiera wbudowanego wsparcia dla YAML, ale mo\u017Cesz u\u017C\
  y\u0107 bibliotek stron trzecich, takich jak `clj-yaml`, do analizowania i generowania\
  \ danych YAML."
title: Praca z YAML
weight: 41
---

## Jak to zrobić:
Clojure nie zawiera wbudowanego wsparcia dla YAML, ale możesz użyć bibliotek stron trzecich, takich jak `clj-yaml`, do analizowania i generowania danych YAML. Najpierw dodaj bibliotekę do zależności projektu:

```clojure
;; Dodaj to do swoich zależności projektu.clj
[clj-yaml "0.7.0"]
```

Oto jak możesz użyć `clj-yaml` do parsowania YAML i konwertowania map Clojure na YAML.

### Parsowanie YAML:
```clojure
(require '[clj-yaml.core :as yaml])

;; Parsowanie łańcucha YAML
(let [yaml-str "name: John Doe\nage: 30\nlanguages:\n  - Clojure\n  - Python"]
  (yaml/parse-string yaml-str))
;; Wynik:
;; => {"name" "John Doe", "age" 30, "languages" ["Clojure" "Python"]}
```

### Generowanie YAML z Clojure:
```clojure
(require '[clj-yaml.core :as yaml])

;; Konwertowanie mapy Clojure na łańcuch YAML
(let [data-map {:name "Jane Doe" :age 28 :languages ["Java" "Ruby"]}]
  (yaml/generate-string data-map))
;; Wynik:
; "age: 28\nlanguages:\n- Java\n- Ruby\nname: Jane Doe\n"
```

Te proste operacje z `clj-yaml` mogą być zintegrowane z aplikacjami Clojure, aby obsługiwać pliki konfiguracyjne lub ułatwić wymianę danych z innymi usługami lub komponentami, które używają YAML.
