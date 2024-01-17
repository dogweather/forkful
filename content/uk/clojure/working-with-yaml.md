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

Зачем и зачем?

YAML (YAML Ain't Markup Language) - це формат даних, який широко використовується у програмуванні для зберігання та обміну даними. Він є читабельним для людей та легко інтерпретованим для комп'ютерів, що робить його улюбленим серед програмістів. Робота з YAML дозволяє зручно створювати та організовувати дані, що досить часто виникає у різних проектах.

Як це зробити:

```Clojure
(def data
    {:person {:name "John"
              :age 27
              :occupation "developer"}})

(require '[hara.data :as string])

(string/to-data "person:
  name: John
  age: 27
  occupation: developer")
=> {:person {:name "John"
             :age 27
             :occupation "developer"}}
```

Глибоке занурення:

YAML був створений у 2001 році для заміни складної і нечитабельної форми XML. Його синтаксис надзвичайно простий та зрозумілий, що дозволяє людям та програмістам легко редагувати та використовувати дані без додаткових інструментів. Існують альтернативи YAML, такі як JSON та CSV, але YAML надає більшу гнучкість та читабельність.

```Clojure
(require '[clojure.data.yaml :as yaml])

;; convert Clojure data to YAML string
(def data
    {:person {:name "John"
              :age 27
              :occupation "developer"}})

(yaml/generate-string data)
=> "person:
      name: John
      age: 27
      occupation: developer"

;; convert YAML string to Clojure data
(def string "person:
                name: Jane
                age: 30
                occupation: designer")

(yaml/parse-string string)
=> {:person {:name "Jane"
             :age 30
             :occupation "designer"}}
```

Дивись також:

Для детальнішої інформації про роботу з YAML в Clojure рекомендується ознайомитись із документацією та прикладами за посиланням: https://github.com/ragnard/cljs-yaml

Для порівняння різних форматів даних та їх використання в різних ситуаціях можна переглянути статтю за посиланням: https://www.smashingmagazine.com/2018/05/json-versus-xml-versus-yaml-which-is-the-best-serialization-format/