---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:20.746260-07:00
description: "\u042F\u043A: Clojure \u043D\u0435 \u043C\u0430\u0454 \u0432\u0431\u0443\
  \u0434\u043E\u0432\u0430\u043D\u043E\u0457 \u043F\u0456\u0434\u0442\u0440\u0438\u043C\
  \u043A\u0438 \u0434\u043B\u044F YAML, \u043E\u0434\u043D\u0430\u043A \u0432\u0438\
  \ \u043C\u043E\u0436\u0435\u0442\u0435 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\
  \u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438 \u0441\u0442\u043E\u0440\u043E\u043D\
  \u043D\u0456 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0438, \u0442\
  \u0430\u043A\u0456 \u044F\u043A `clj-yaml` \u0434\u043B\u044F \u0430\u043D\u0430\
  \u043B\u0456\u0437\u0443 \u0442\u0430 \u0433\u0435\u043D\u0435\u0440\u0430\u0446\
  \u0456\u0457 \u0434\u0430\u043D\u0438\u0445\u2026"
lastmod: '2024-03-13T22:44:48.688029-06:00'
model: gpt-4-0125-preview
summary: "Clojure \u043D\u0435 \u043C\u0430\u0454 \u0432\u0431\u0443\u0434\u043E\u0432\
  \u0430\u043D\u043E\u0457 \u043F\u0456\u0434\u0442\u0440\u0438\u043C\u043A\u0438\
  \ \u0434\u043B\u044F YAML, \u043E\u0434\u043D\u0430\u043A \u0432\u0438 \u043C\u043E\
  \u0436\u0435\u0442\u0435 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\
  \u0443\u0432\u0430\u0442\u0438 \u0441\u0442\u043E\u0440\u043E\u043D\u043D\u0456\
  \ \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0438, \u0442\u0430\u043A\
  \u0456 \u044F\u043A `clj-yaml` \u0434\u043B\u044F \u0430\u043D\u0430\u043B\u0456\
  \u0437\u0443 \u0442\u0430 \u0433\u0435\u043D\u0435\u0440\u0430\u0446\u0456\u0457\
  \ \u0434\u0430\u043D\u0438\u0445 YAML."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 YAML"
weight: 41
---

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
