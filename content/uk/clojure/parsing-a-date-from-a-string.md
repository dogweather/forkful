---
title:                "Аналіз дати з рядка"
date:                  2024-01-20T15:35:46.863428-07:00
html_title:           "Arduino: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"

category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Парсинг дати зі строки — це процес конвертації текстового представлення дати до структури даних, яку програма може зрозуміти і обробити. Це потрібно, щоб можна було легко маніпулювати датами та виконувати з ними операції, як-от додавання днів або порівняння.

## How to: (Як це зробити:)
```Clojure
(require '[clj-time.coerce :as coerce]
         '[clj-time.format :as format])

;; Визначаємо формат дати
(def my-formatter (format/formatters :basic-date-time))

;; Парсинг зі строки в дату
(def my-date-str "20230415T123000Z")
(def parsed-date (coerce/from-string my-formatter my-date-str))

;; Виведення результату
(println parsed-date)
```
Sample output:
```
2023-04-15T12:30:00.000Z
```

## Deep Dive (Занурення у Деталі):
Давніше, парсинг дат у Clojure часто здійснювався за допомогою Java Date APIs через Java Interop. Це було не завжди ідеально. З'явилася бібліотека `clj-time`, основана на Joda Time, яка значно спростила роботу з датами у Clojure.

Крім `clj-time`, можна використовувати інші бібліотеки, як `java-time`, яка більш характерна для нових версій Java.

Щодо імплементації - Clojure відмінно інтегрується з Java, тому часто використовується обгортання Java бібліотек. Це дає простоту синтаксису Clojure та велику можливість Java екосистеми.

## See Also (Дивіться також):
- [clj-time GitHub repository](https://github.com/clj-time/clj-time)
- [java-time GitHub repository](https://github.com/dm3/clojure.java-time)
- [ClojureDocs - a community-powered documentation and examples repository for Clojure](https://clojuredocs.org/)
- [The Joda-Time project](https://www.joda.org/joda-time/)
