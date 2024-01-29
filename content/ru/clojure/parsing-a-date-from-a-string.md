---
title:                "Анализ даты из строки"
date:                  2024-01-28T23:59:57.366081-07:00
model:                 gpt-4-0125-preview
simple_title:         "Анализ даты из строки"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/clojure/parsing-a-date-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?

Разбор даты из строки означает преобразование текста даты, понятного человеку, в формат, который понимает компьютер. Программисты делают это потому, что компьютеры предпочитают даты в виде чисел для сортировки, хранения или манипулирования.

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
