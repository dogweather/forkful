---
title:                "Clojure: Робота з yaml"
simple_title:         "Робота з yaml"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## Чому

Мова програмування Clojure стає все більш популярною серед розробників усього світу. Один з головних принципів Clojure - простота та лаконічність. Як результат, ця мова добре підходить для роботи з файлами у форматі YAML.

## Як працювати з YAML в Clojure

Clojure має вбудовану бібліотеку для роботи з YAML - clojure.yaml. Це дозволяє легко завантажувати та зберігати дані в цьому форматі.

```Clojure
(require '[clojure.yaml :as yaml])

(def data {:language "Clojure"
           :awesome true
           :reasons ["simple" "concise" "functional"]})

(def content (yaml/generate-string data))
```

В цьому прикладі ми створюємо змінну data з даними у форматі hash map, а потім використовуємо бібліотеку clojure.yaml для їх конвертації у рядок. Надалі цей рядок можна зберегти у файл або передати у вигляді вихідних даних.

```Clojure
(def parsed-data (yaml/parse-string content))
=> {:language "Clojure", :awesome true, :reasons ["simple" "concise" "functional"]}
```

За допомогою функції parse-string ми можемо отримати структуру даних з рядка YAML. Також можна використовувати функції load для завантаження даних з файлу або parse для роботи з рядком у форматі YAML.

## Поглиблене вивчення

YAML - це формат даних, який використовує звертання та дуже схожий на JSON. Він дуже зручний для зберігання та передачі структурованих даних. Для більш поглибленого вивчення рекомендуємо ознайомитися з офіційною документацією по бібліотеці clojure.yaml та документацією по YAML.

## Дивись також

- [Офіційна документація clojure.yaml](https://clojure.github.io/clojure/clojure.yaml-api.html)
- [Офіційна документація YAML](https://yaml.org/)
- [Примери роботи з YAML в Clojure](https://github.com/yaml/yaml-clojure/tree/master/examples)