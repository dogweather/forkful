---
title:                "Робота з YAML"
date:                  2024-01-19
html_title:           "Arduino: Робота з YAML"
simple_title:         "Робота з YAML"

category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## Що це таке і навіщо?
YAML - це формат серіалізації даних, читабельний для людей. Програмісти використовують його для конфігураційних файлів та обміну даними завдяки його простоті та лаконічності.

## Як це зробити:
```Clojure
(require '[clj-yaml.core :as yaml])

; Зчитування YAML
(def config-string "
http:
  port: 80
  hostname: example.com
")

(def config-map (yaml/parse-string config-string))
(println config-map)

; Вивід: {:http {:port 80, :hostname "example.com"}}

; Запис YAML
(def config-data 
  {:database {:user "dbuser", :password "dbpass"}})

(println (yaml/generate-string config-data))

; Вивід: "database:\n  user: dbuser\n  password: dbpass\n"
```

## Поглиблене вивчення:
YAML (YAML Ain't Markup Language) з'явився в 2001 році і позиціонувався як легша альтернатива XML. Альтернативи YAML - це JSON і TOML. У Clojure для роботи з YAML можна використовувати бібліотеку `clj-yaml`, яка ґрунтується на Java-бібліотеці `snakeyaml`. В Clojure важливо управління залежностями, тому не забувайте вказати бібліотеку у файлі `project.clj` або `deps.edn`.

## Більше інформації:
- Clojure офіційна сторінка: https://clojure.org
- `clj-yaml` бібліотека на GitHub: https://github.com/clj-commons/clj-yaml
- Як працювати з Leiningen та `project.clj`: https://leiningen.org
- Ресурси для вивчення Clojure: https://clojurecademy.com
