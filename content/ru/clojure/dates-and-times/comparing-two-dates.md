---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:57.489276-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: Clojure \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u0442\
  \ \u0432\u043E\u0437\u043C\u043E\u0436\u043D\u043E\u0441\u0442\u0438 \u0432\u0437\
  \u0430\u0438\u043C\u043E\u0434\u0435\u0439\u0441\u0442\u0432\u0438\u044F \u0441\
  \ Java \u0434\u043B\u044F \u0440\u0430\u0431\u043E\u0442\u044B \u0441 \u0434\u0430\
  \u0442\u0430\u043C\u0438. \u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0437\u0430\
  \u043A\u0430\u0442\u0430\u0435\u043C \u0440\u0443\u043A\u0430\u0432\u0430 \u0438\
  \ \u043D\u044B\u0440\u043D\u0435\u043C \u0432 \u044D\u0442\u043E."
lastmod: '2024-03-13T22:44:44.373085-06:00'
model: gpt-4-0125-preview
summary: "Clojure \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u0442 \u0432\
  \u043E\u0437\u043C\u043E\u0436\u043D\u043E\u0441\u0442\u0438 \u0432\u0437\u0430\u0438\
  \u043C\u043E\u0434\u0435\u0439\u0441\u0442\u0432\u0438\u044F \u0441 Java \u0434\u043B\
  \u044F \u0440\u0430\u0431\u043E\u0442\u044B \u0441 \u0434\u0430\u0442\u0430\u043C\
  \u0438."
title: "\u0421\u0440\u0430\u0432\u043D\u0435\u043D\u0438\u0435 \u0434\u0432\u0443\u0445\
  \ \u0434\u0430\u0442"
weight: 27
---

## Как это сделать:
Clojure использует возможности взаимодействия с Java для работы с датами. Давайте закатаем рукава и нырнем в это:

```clojure
;; Импорт класса Date из Java
(import java.util.Date)

;; Создание двух экземпляров даты
(def date1 (java.util.Date.))
(Thread/sleep 1000) ;; Подождем немного
(def date2 (java.util.Date.))

;; Сравнение дат
(println (.before date1 date2)) ; true, date1 раньше date2
(println (.after date1 date2))  ; false, date1 не после date2
(println (.equals date1 date2)) ; false, date1 не та же самая, что и date2
```

Пример вывода может выглядеть так, но с другими временными метками:

```
true
false
false
```

## Глубокое погружение
Ранее разработчики на Clojure часто использовали `Date` из Java для операций с датами, вызывая методы с использованием оператора точки, как было показано выше. Альтернативы включают `clj-time`, библиотеку Clojure, которая оборачивает Joda-Time.

Пример использования `clj-time` выглядит следующим образом:

```clojure
;; Добавьте clj-time в зависимости вашего проекта
(require '[clj-time.core :as time])
(require '[clj-time.coerce :as coerce])

;; Создание двух экземпляров date-time
(def date-time1 (time/now))
(Thread/sleep 1000) ;; Подождем секунду
(def date-time2 (time/now))

;; Сравнение с использованием функций clj-time
(println (time/before? date-time1 date-time2)) ; true
(println (time/after? date-time1 date-time2))  ; false
(println (time/equal? date-time1 date-time2))  ; false
```

Позиция Clojure по времени заключается в использовании библиотек Java, в то время как clj-time интегрируется с Joda-Time для более идиоматического опыта Clojure.

Начиная с Java 8, пакет `java.time` — вдохновленный Joda-Time — является предпочтительным способом работы с датами и временем в Java и, соответственно, в Clojure через взаимодействие. Улучшенный дизайн и дополнительные возможности, такие как часовые пояса, делают `java.time` надежным выбором.

## Смотрите также
- [Взаимодействие Clojure с Java](https://clojure.org/reference/java_interop)
- [Репозиторий clj-time на GitHub](https://github.com/clj-time/clj-time)
- [Руководство по API даты и времени Java](https://docs.oracle.com/javase/tutorial/datetime/)
