---
title:                "Робота з yaml"
html_title:           "Clojure: Робота з yaml"
simple_title:         "Робота з yaml"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## Чому

 YAML - це формат файлу, який досить часто використовується для зберігання і обміну данними у багатьох програмах та розробці програмного забезпечення. Ця стаття допоможе вам дізнатись про базові концепції YAML та як використовувати його в своїх проектах на Clojure.

## Як

Основними структурами даних у YAML є масиви, асоціативні масиви та скалярні значення. Давайте розглянемо деякі приклади використання YAML у Clojure.

### Створення асоціативного масиву:

```Clojure
;; Використовуємо конструктор "hash-map" для створення асоціативного масиву
(def my-map (hash-map :key1 "значення1" :key2 "значення2"))
```

### Додавання нового елементу до асоціативного масиву:

```Clojure
;; Використовуємо функцію "assoc" для додавання нового елементу до масиву
(assoc my-map :key3 "значення3")
```

### Отримання значення за ключем:

```Clojure
;; Використовуємо функцію "get" для отримання значення за ключем
(get my-map :key2)
```

### Перетворення YAML у структури даних Clojure:

```Clojure
;; Використовуємо бібліотеку "cheshire" для перетворення YAML у структури даних Clojure
(require '[cheshire.core :as json])

(-> "my_yaml_file.yaml"
    slurp
    (json/parse-string :key-fn keyword))
```

## Глибше

YAML - це розширений формат для структурування та зберігання даних у зручному для читання та редагування вигляді. Детальніше про його синтаксис можна дізнатись у [офіційній документації](https://yaml.org/spec/1.2/spec.html). Також, у Clojure є багато інших бібліотек для роботи з YAML, наприклад [data.yaml](https://github.com/yogthos/data.yaml) та [yaml-clojure](https://github.com/lrhn/yaml-clojure).

## Дивіться також

[Офіційна документація Clojure](https://clojure.org/documentation)

[Clojure Cookbook: Serializing Data to and from YAML](https://clojure.org/cookbook/serialization/yaml)