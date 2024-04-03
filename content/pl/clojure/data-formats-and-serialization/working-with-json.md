---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:11.647091-07:00
description: "Praca z JSON (JavaScript Object Notation) w Clojure polega na parsowaniu\
  \ ci\u0105g\xF3w JSON do struktur danych Clojure (mapy, wektory) i odwrotnie. To\
  \ zadanie\u2026"
lastmod: '2024-03-13T22:44:35.018632-06:00'
model: gpt-4-0125-preview
summary: "Praca z JSON (JavaScript Object Notation) w Clojure polega na parsowaniu\
  \ ci\u0105g\xF3w JSON do struktur danych Clojure (mapy, wektory) i odwrotnie."
title: Praca z JSON
weight: 38
---

## Jak to zrobić:
Clojure nie zawiera wbudowanych funkcji do pracy z JSON, więc typowo używa się bibliotek firm trzecich. `cheshire` i `jsonista` to popularne wybory ze względu na ich łatwość użycia i wydajność.

### Korzystanie z Cheshire
Pierwsze, dodaj Cheshire do zależności swojego projektu w `project.clj`:
```clj
[com.fasterxml.jackson.core/jackson-core "2.12.0"]
[cheshire "5.10.1"]
```

Aby sparsować ciąg JSON do mapy Clojure i przekształcić mapę w ciąg JSON:

```clj
(require '[cheshire.core :as json])

;; Parsowanie ciągu JSON do mapy Clojure
(let [json-input "{\"name\":\"John\", \"age\":30}"]
  (json/parse-string json-input true)) ; => {"name" "John", "age" 30}

;; Konwersja mapy Clojure do ciągu JSON
(let [clj-map {"name" "John", "age" 30}]
  (json/generate-string clj-map)) ; => "{\"name\":\"John\",\"age\":30}"
```

### Korzystanie z Jsonista
Dodaj Jsonista do swojego projektu `project.clj`:
```clj
[jsonista "0.3.2"]
```

Podobne operacje z Jsonista:

```clj
(require '[jsonista.core :as j])

;; Parsowanie ciągu JSON do Clojure
(let [json-input "{\"name\":\"Emily\", \"age\":25}"]
  (j/read-value json-input)) ; => {"name" "Emily", "age" 25}

;; Konwersja mapy Clojure do ciągu JSON
(let [clj-map {"name" "Emily", "age" 25}]
  (j/write-value-as-string clj-map)) ; => "{\"name\":\"Emily\",\"age\":25}"
```

W obu bibliotekach masz możliwość kodowania i dekodowania bardziej złożonych struktur danych, a także są dostępne dodatkowe funkcje i parametry, które pozwalają na dostosowanie procesów serializacji i deserializacji. Dla większości aplikacji, przedstawiona funkcjonalność zapewnia solidną podstawę do pracy z JSON w aplikacjach Clojure.
