---
title:                "Работа с YAML"
date:                  2024-01-29T00:05:10.124807-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с YAML"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/clojure/working-with-yaml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?

YAML, что означает "YAML Ain't Markup Language" (YAML - это не язык разметки), представляет собой дружественный к человеку стандарт сериализации данных для всех языков программирования. Программисты используют YAML для файлов конфигурации и обмена данными, когда важно удобство чтения.

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
