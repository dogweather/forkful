---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:05.596768-07:00
description: "YAML, rekurencyjny akronim od \"YAML Ain't Markup Language\" (YAML nie\
  \ jest j\u0119zykiem znacznik\xF3w), to format serializacji danych czytelny dla\
  \ cz\u0142owieka,\u2026"
lastmod: '2024-02-25T18:49:33.441083-07:00'
model: gpt-4-0125-preview
summary: "YAML, rekurencyjny akronim od \"YAML Ain't Markup Language\" (YAML nie jest\
  \ j\u0119zykiem znacznik\xF3w), to format serializacji danych czytelny dla cz\u0142\
  owieka,\u2026"
title: Praca z YAML
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
