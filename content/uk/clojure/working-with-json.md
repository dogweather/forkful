---
title:                "Clojure: Робота з json"
simple_title:         "Робота з json"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/working-with-json.md"
---

{{< edit_this_page >}}

# Чому

JSON є популярним форматом даних для обміну із сучасними веб-додатками, тому те що розуміти та уміти працювати з ним - важливий навик для будь-якого програміста, який хоче створювати веб-сайти та додатки.

# Як

```Clojure
;; Імпортуємо бібліотеку для роботи з JSON
(require '[clojure.data.json :as json])

;; Створюємо звичайний Clojure хеш-мап, який містить дані
(def data {"name" "John" "age" 25 "hobbies" ["reading" "traveling" "cooking"]})

;; Конвертуємо дані у формат JSON
(def json-data (json/write-str data))

;; Виводимо результат у консоль
(clojure.pprint/pprint json-data) 
```

Вивід:
``` Clojure
"{\"name\":\"John\",\"age\":25,\"hobbies\":[\"reading\",\"traveling\",\"cooking\"]}"
```

# Глибокий занурення

JSON - це структурований формат даних, який легко інтерпретується людиною та комп'ютером. У Clojure є багато бібліотек для роботи з JSON, які допоможуть вам зчитувати та створювати дані у цьому форматі.

Одна з найпопулярніших бібліотек - **cheshire**, яка пропонує зручні функції для роботи з JSON. За допомогою функцій `parse-string` і `generate-string` ви можете зчитувати та створювати дані з JSON, а також ви можете використовувати функцію `generate-stream` для збереження об'єктів у файл.

# Запитайте

- [Офіційна документація по бібліотеці cheshire](https://github.com/dakrone/cheshire)
- [Робота з JSON у Clojure](https://cemerick.com/2011/07/11/json-io/)
- [Пакет обробки JSON даних - jsonista](https://github.com/metosin/jsonista)

# Дивіться також

- [Як працювати з XML у Clojure](https://example.com) (soon to come)