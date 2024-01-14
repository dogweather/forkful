---
title:                "Clojure: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Чому

Час тікає швидко і нам, як програмістам, часто потрібно знати поточну дату для ведення обліку часу. Дістати поточну дату в програмуванні не така складна задача, як може здатися на перший погляд.

## Як отримати поточну дату

Існує багато вбудованих функцій в мові Clojure, які дозволяють нам отримувати поточну дату і час. Наприклад, для отримання поточної дати можна використовувати функцію ```clojure
    (require '[java.time.LocalDateTime :refer [now]])
    (now)
```

Це поверне об'єкт, який містить поточну дату та час у форматі LocalDateTime. Якщо вам потрібно отримати лише дату без часу, ви можете скористатися функцією ```clojure
    (today)
```

Також ви можете використовувати різні методи і властивості, щоб отримати конкретну інформацію про дату, наприклад, день, місяць або рік. Ось приклад використання таких методів для отримання поточного місяця:

```clojure
    (require '[java.time.LocalDateTime :refer [now]])
    (def current-month (.getMonth (now)))
    (println current-month)
```

Вивід цього коду буде виглядати приблизно так: "SEPTEMBER"

## Глибоке занурення

У функції ```clojure (now)``` є багато властивостей, які можна використовувати для отримання поточної дати та часу в різних форматах. Наприклад, ви можете отримати поточну зону часу за допомогою методу ```clojure (.getZone (now))```. Також можливо створити свій власний об'єкт LocalDateTime і встановити його значення дати та часу за допомогою методів ```clojure (.withYear 2019)```, ```clojure (.withMonth 9)```, тощо.

## Дивіться також

- [Документація Clojure для java.time.LocalDateTime](https://clojure.github.io/api/java.time/java.time.LocalDateTime.html)
- [Зобра