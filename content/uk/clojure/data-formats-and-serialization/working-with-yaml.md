---
title:                "Робота з YAML"
aliases:
- /uk/clojure/working-with-yaml.md
date:                  2024-02-03T19:25:20.746260-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та Чому?

YAML, рекурсивний акронім "YAML Ain't Markup Language" (YAML - це не мова розмітки), є форматом серіалізації даних, зрозумілим для людини, який використовується для файлів конфігурації та обміну даними між мовами з різними структурами даних. Програмісти використовують YAML через його простоту та читабельність, що робить його ідеальним вибором для конфігурації додатків та сприяння обміну даними в поліглотних програмних середовищах.

## Як:

Clojure не має вбудованої підтримки для YAML, однак ви можете використовувати сторонні бібліотеки, такі як `clj-yaml` для аналізу та генерації даних YAML. Спочатку додайте бібліотеку до залежностей вашого проекту:

```clojure
;; Додайте це до залежностей вашого project.clj
[clj-yaml "0.7.0"]
```

Ось як ви можете використовувати `clj-yaml` для аналізу YAML та конвертації мап Clojure в YAML.

### Аналіз YAML:

```clojure
(require '[clj-yaml.core :as yaml])

;; Аналіз рядка YAML
(let [yaml-str "name: John Doe\nage: 30\nlanguages:\n  - Clojure\n  - Python"]
  (yaml/parse-string yaml-str))
;; Вивід:
;; => {"name" "John Doe", "age" 30, "languages" ["Clojure" "Python"]}
```

### Генерація YAML з Clojure:

```clojure
(require '[clj-yaml.core :as yaml])

;; Конвертація мапи Clojure в рядок YAML
(let [data-map {:name "Jane Doe" :age 28 :languages ["Java" "Ruby"]}]
  (yaml/generate-string data-map))
;; Вивід:
; "age: 28\nlanguages:\n- Java\n- Ruby\nname: Jane Doe\n"
```

Ці прості операції з `clj-yaml` можна інтегрувати в додатки Clojure для обробки файлів конфігурації або сприяння обміну даними з іншими сервісами чи компонентами, що використовують YAML.
