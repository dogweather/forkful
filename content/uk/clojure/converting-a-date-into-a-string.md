---
title:                "Перетворення дати в рядок"
html_title:           "Lua: Перетворення дати в рядок"
simple_title:         "Перетворення дати в рядок"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Що і чому? 

Перетворення дати в рядок - це процес приведення дати до текстового формату. Програмісти роблять це, щоб полегшити зчитування та обробку даних.

## Як це зробити:

Clojure надає зручний спосіб конвертації дати в рядок за допомогою вбудованої бібліотеки `java.time`. Ось приклад:

```Clojure
(require '[clojure.java-time :as jt])

(let [current-date (jt/local-date)]
  (jt/format current-date))
```

Виконуємо:

```Clojure
"2022-09-22"
```

## Поглиблений аналіз:

1. **Історичний контекст**: Конвертація дати в рядок є поширеною задачею в програмуванні з самого її початку. Вона допомагає у стандартизації формату дати та їх збереженні.

2. **Альтернативи**: Існують інші методи конвертації дати в рядок в Clojure, такі як `clj-time` або `java.text.SimpleDateFormat`. Але `java.time` є найбільш сучасним і гнучким аналогом.

3. **Деталі реалізації**: `java.time` використовує ISO стандарт для формату дати й часу. Ви можете налаштувати свій формат, передавши рядок з відповідним шаблоном до методу `format`. 

## Дивіться також:

1. [Clojure - java.time](https://clojure.github.io/clojure/clojure.java-time-api.html)
2. [Java DateTime tutorial](https://www.baeldung.com/java-8-date-time-intro)
3. [ISO 8601 Date Format](https://en.wikipedia.org/wiki/ISO_8601)