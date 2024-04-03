---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:10.124807-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: Clojure \u0438\u0437\u043D\u0430\u0447\u0430\u043B\u044C\u043D\u043E\
  \ \u043D\u0435 \u043F\u043E\u0434\u0434\u0435\u0440\u0436\u0438\u0432\u0430\u0435\
  \u0442 \u0440\u0430\u0431\u043E\u0442\u0443 \u0441 YAML. \u0412\u0430\u043C \u043F\
  \u043E\u0442\u0440\u0435\u0431\u0443\u0435\u0442\u0441\u044F \u0438\u0441\u043F\u043E\
  \u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C \u0431\u0438\u0431\u043B\u0438\u043E\
  \u0442\u0435\u043A\u0443, \u043D\u0430\u043F\u0440\u0438\u043C\u0435\u0440, `clj-yaml`.\
  \ \u0421\u043D\u0430\u0447\u0430\u043B\u0430 \u0434\u043E\u0431\u0430\u0432\u044C\
  \u0442\u0435 \u0435\u0451 \u0432\u2026"
lastmod: '2024-03-13T22:44:44.387398-06:00'
model: gpt-4-0125-preview
summary: "Clojure \u0438\u0437\u043D\u0430\u0447\u0430\u043B\u044C\u043D\u043E \u043D\
  \u0435 \u043F\u043E\u0434\u0434\u0435\u0440\u0436\u0438\u0432\u0430\u0435\u0442\
  \ \u0440\u0430\u0431\u043E\u0442\u0443 \u0441 YAML."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 YAML"
weight: 41
---

## Как это сделать:
Clojure изначально не поддерживает работу с YAML. Вам потребуется использовать библиотеку, например, `clj-yaml`. Сначала добавьте её в зависимости:

```clojure
;; Добавить в project.clj или deps.edn
[clj-yaml "0.7.0"]
```

Теперь давайте разберёмся, как преобразовать строку YAML в карту Clojure и обратно:

```clojure
(require '[clj-yaml.core :as yaml])

;; Преобразование строки YAML в карту Clojure
(let [yaml-str "foo: bar\nbaz: 42"]
  (yaml/parse-string yaml-str))
;; => {"foo" "bar", "baz" 42}

;; Преобразование карты Clojure в YAML
(let [clojure-map {"foo" "bar", "baz" 42}]
  (yaml/generate-string clojure-map))
;; Выводит строку YAML:
;; foo: bar
;; baz: 42
```

## Погружение в детали
YAML был впервые выпущен в 2001 году с целью быть более читаемым, чем XML, при этом предлагая богаче структуры данных, чем JSON. `clj-yaml` построена на основе SnakeYAML, библиотеки Java, что позволяет обеспечивать взаимодействие с языками JVM. Альтернативы включают прямое использование `org.yaml.snakeyaml` или `cheshire` для преобразования в JSON, поскольку JSON является подмножеством YAML.

## Смотрите также
Углубитесь в тему с помощью этих ресурсов:

- Официальный сайт YAML: [https://yaml.org](https://yaml.org)
- Github для clj-yaml: [https://github.com/clj-commons/clj-yaml](https://github.com/clj-commons/clj-yaml)
- Движок SnakeYAML: [https://bitbucket.org/asomov/snakeyaml-engine](https://bitbucket.org/asomov/snakeyaml-engine)
