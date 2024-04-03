---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:59:57.366081-07:00
description: "\u041A\u0430\u043A: Clojure \u0438\u0441\u043F\u043E\u043B\u044C\u0437\
  \u0443\u0435\u0442 \u0432\u043E\u0437\u043C\u043E\u0436\u043D\u043E\u0441\u0442\u0438\
  \ Java \u0434\u043B\u044F \u0440\u0430\u0437\u0431\u043E\u0440\u0430 \u0434\u0430\
  \u0442, \u043F\u043E\u044D\u0442\u043E\u043C\u0443 \u043C\u044B \u0431\u0443\u0434\
  \u0435\u043C \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C\
  \ \u0437\u0434\u0435\u0441\u044C `java.time.LocalDate`."
lastmod: '2024-03-13T22:44:44.367478-06:00'
model: gpt-4-0125-preview
summary: "Clojure \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u0442 \u0432\
  \u043E\u0437\u043C\u043E\u0436\u043D\u043E\u0441\u0442\u0438 Java \u0434\u043B\u044F\
  \ \u0440\u0430\u0437\u0431\u043E\u0440\u0430 \u0434\u0430\u0442, \u043F\u043E\u044D\
  \u0442\u043E\u043C\u0443 \u043C\u044B \u0431\u0443\u0434\u0435\u043C \u0438\u0441\
  \u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C \u0437\u0434\u0435\u0441\
  \u044C `java.time.LocalDate`."
title: "\u0410\u043D\u0430\u043B\u0438\u0437 \u0434\u0430\u0442\u044B \u0438\u0437\
  \ \u0441\u0442\u0440\u043E\u043A\u0438"
weight: 30
---

## Как:
Clojure использует возможности Java для разбора дат, поэтому мы будем использовать здесь `java.time.LocalDate`:

```Clojure
(require '[java-time :as jt])

(defn parse-date [date-str]
  (jt/local-date "yyyy-MM-dd" date-str))

(println (parse-date "2023-04-05"))
```

Вывод:

```
#object[java.time.LocalDate 0x4b121a5e "2023-04-05"]
```

Здесь `java-time` – это библиотека Clojure, которая оборачивает API `java.time`. Это более идиоматичный Clojure, чем непосредственное взаимодействие с Java.

## Погружение
Clojure, появившийся в 2007 году, – это современный Lisp, работающий на JVM. Он предоставляет взаимодействие с Java, включая обработку дат. До появления `java.time` (введено в Java 8) в Java использовались `java.util.Date` и `java.text.SimpleDateFormat`, которые были громоздкими и менее безопасными для потоков.

`clj-time`, оболочка Joda-Time, была популярна в Clojure до `java-time`, но теперь Joda-Time считается устаревшей. Сегодня `java-time` – это основной выбор, поскольку он оборачивает пакет `java.time`, который гораздо лучше и по умолчанию неизменяем.

Существуют и чисто Clojure библиотеки, такие как `tick`, но они также строятся на основе `java.time` Java по практическим соображениям. Используемый пакет `java.time` использует систему календаря ISO, но поддерживает и другие. Такая гибкость означает, что программы на Clojure не только дружелюбны к JVM, но и готовы к международному использованию.

## См. также
- [Документация Clojure](https://clojure.org/)
- [Библиотека java-time](https://github.com/dm3/clojure.java-time)
- [Старая библиотека clj-time](https://github.com/clj-time/clj-time)
- [Java SE Date Time](https://docs.oracle.com/javase/tutorial/datetime/)

Продолжайте исследования и счастливого кодирования!
