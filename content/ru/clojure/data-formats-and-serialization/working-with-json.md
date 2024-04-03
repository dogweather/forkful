---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:15.188531-07:00
description: "JSON (JavaScript Object Notation) - \u044D\u0442\u043E \u0444\u043E\u0440\
  \u043C\u0430\u0442 \u0434\u0430\u043D\u043D\u044B\u0445, \u0438\u0441\u043F\u043E\
  \u043B\u044C\u0437\u0443\u0435\u043C\u044B\u0439 \u0434\u043B\u044F \u0445\u0440\
  \u0430\u043D\u0435\u043D\u0438\u044F \u0438 \u043F\u0435\u0440\u0435\u0434\u0430\
  \u0447\u0438 \u0434\u0430\u043D\u043D\u044B\u0445. \u041F\u0440\u043E\u0433\u0440\
  \u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0438\u0441\u043F\u043E\u043B\u044C\u0437\
  \u0443\u044E\u0442 JSON \u0438\u0437-\u0437\u0430 \u0435\u0433\u043E \u0443\u0434\
  \u043E\u0431\u0441\u0442\u0432\u0430 \u043F\u0440\u0438\u2026"
lastmod: '2024-03-13T22:44:44.388976-06:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) - \u044D\u0442\u043E \u0444\u043E\u0440\
  \u043C\u0430\u0442 \u0434\u0430\u043D\u043D\u044B\u0445, \u0438\u0441\u043F\u043E\
  \u043B\u044C\u0437\u0443\u0435\u043C\u044B\u0439 \u0434\u043B\u044F \u0445\u0440\
  \u0430\u043D\u0435\u043D\u0438\u044F \u0438 \u043F\u0435\u0440\u0435\u0434\u0430\
  \u0447\u0438 \u0434\u0430\u043D\u043D\u044B\u0445."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 JSON"
weight: 38
---

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
