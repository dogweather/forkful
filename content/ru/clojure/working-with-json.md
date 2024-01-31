---
title:                "Работа с JSON"
date:                  2024-01-29T00:04:15.188531-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/clojure/working-with-json.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
JSON (JavaScript Object Notation) - это формат данных, используемый для хранения и передачи данных. Программисты используют JSON из-за его удобства при работе с веб-API и независимого от языка текстового формата, что делает его очень удобным для обмена данными.

## Как это сделать:
Давайте поиграем с JSON в Clojure. Вам понадобится `Cheshire`, популярная библиотека для кодирования/декодирования JSON.

Сначала добавьте Cheshire в зависимости вашего `project.clj`:
```clojure
[cheshire "5.10.1"]
```

Чтение JSON из строки и преобразование его в карту Clojure:
```clojure
(require '[cheshire.core :as json])

(def json-str "{\"name\":\"Clojure\"}")
(def clojure-map (json/parse-string json-str))

(println clojure-map)  ;; => {"name" "Clojure"}
```

Преобразование карты Clojure в строку JSON:
```clojure
(def clojure-data {:language "Clojure" :cool true})
(def json-output (json/generate-string clojure-data))

(println json-output)  ;; => {"language":"Clojure","cool":true}
```

Разбор JSON из файла:
```clojure
(slurp "data.json")  ;; содержимое: {"message": "Привет, JSON!"}
(def file-content (slurp "data.json"))
(def message-data (json/parse-string file-content true))

(println message-data)  ;; => {"message" "Привет, JSON!"}
```

## Погружение
История JSON начинается с JavaScript, но теперь он везде, не завися от своего родительского языка. Альтернативы? Раньше выбором был XML, более многословный, правда. YAML проще, более дружелюбный для человека, но не такой универсальный для API. С точки зрения реализации: Clojure - не JavaScript, поэтому библиотеки вроде Cheshire жизненно важны. Они служат мостом, используя под капотом Java-библиотеки для эффективного выполнения разбора и генерации.

## Смотрите также
- [Репозиторий Cheshire на GitHub](https://github.com/dakrone/cheshire): Детали библиотеки и обновления.
- [JSON.org](https://www.json.org): Спецификации и детали JSON.
- [Clojure с нуля: JSON](https://aphyr.com/posts/305-clojure-from-the-ground-up-json): Подробное руководство по работе с JSON в Clojure.
