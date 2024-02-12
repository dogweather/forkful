---
title:                "Отримання поточної дати"
aliases: - /uk/clojure/getting-the-current-date.md
date:                  2024-02-03T19:09:28.930041-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отримання поточної дати"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та Чому?
Отримання поточної дати в програмуванні є критично важливим з безлічі причин, включаючи логування, ставлення часових міток подій і планування завдань. У Clojure, діалекті Lisp на JVM, це завдання використовує можливості інтероперабельності з Java, що дозволяє безпроблемний доступ до багатого Java Date-Time API.

## Як:

### Використання інтероперабельності з Java
Безшовна інтероперабельність Clojure з Java дозволяє вам безпосередньо використовувати Java Date-Time API. Ось як ви можете отримати поточну дату:

```clojure
(import java.time.LocalDate)

(defn get-current-date []
  (str (LocalDate/now)))

;; Приклад виводу
(get-current-date) ; "2023-04-15"
```

### Використання бібліотеки clj-time
Для більш ідіоматичного рішення у Clojure ви можете скористатися бібліотекою `clj-time`, обгорткою навколо Joda-Time, хоча для більшості нових проектів рекомендується вбудований Java 8 Date-Time API. Проте, якщо вам подобається або потрібна `clj-time`:

Спочатку, додайте `clj-time` до залежностей вашого проекту. У вашому `project.clj` включіть:

```clojure
[clj-time "0.15.2"]
```

Далі, використовуйте її для отримання поточної дати:

```clojure
(require '[clj-time.core :as time])

(defn get-current-date-clj-time []
  (str (time/now)))

;; Приклад виводу
(get-current-date-clj-time) ; "2023-04-15T12:34:56.789Z"
```

Обидва методи надають швидкі, ефективні способи отримання поточної дати в Clojure, використовуючи потужність основної платформи Java або зручність бібліотеки специфічної для Clojure.
