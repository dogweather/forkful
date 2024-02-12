---
title:                "Praca z YAML"
aliases:
- /pl/clojure/working-with-yaml/
date:                  2024-02-03T19:25:05.596768-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

YAML, rekurencyjny akronim od "YAML Ain't Markup Language" (YAML nie jest językiem znaczników), to format serializacji danych czytelny dla człowieka, używany do plików konfiguracyjnych oraz wymiany danych między językami o różnych strukturach danych. Programiści wykorzystują YAML ze względu na jego prostotę i czytelność, co czyni go idealnym wyborem do konfiguracji aplikacji i ułatwienia wymiany danych w środowiskach programowania poliglotycznego.

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
