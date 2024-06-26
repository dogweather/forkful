---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:00:43.107925-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412 Clojure \u0434\u043B\u044F \u0432\u044B\u0432\u043E\u0434\u0430\
  \ \u043E\u0442\u043B\u0430\u0434\u043E\u0447\u043D\u043E\u0439 \u0438\u043D\u0444\
  \u043E\u0440\u043C\u0430\u0446\u0438\u0438 \u0447\u0430\u0441\u0442\u043E \u0438\
  \u0441\u043F\u043E\u043B\u044C\u0437\u0443\u044E\u0442\u0441\u044F `println`, `printf`,\
  \ `pr`, \u0438\u043B\u0438 `prn`. \u0412\u043E\u0442 \u043A\u0430\u043A \u0432\u044B\
  \ \u043C\u043E\u0436\u0435\u0442\u0435 \u0434\u043E\u0431\u0430\u0432\u0438\u0442\
  \u044C \u0432 \u043A\u043E\u0434\u2026"
lastmod: '2024-03-13T22:44:44.354529-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Clojure \u0434\u043B\u044F \u0432\u044B\u0432\u043E\u0434\u0430 \u043E\
  \u0442\u043B\u0430\u0434\u043E\u0447\u043D\u043E\u0439 \u0438\u043D\u0444\u043E\u0440\
  \u043C\u0430\u0446\u0438\u0438 \u0447\u0430\u0441\u0442\u043E \u0438\u0441\u043F\
  \u043E\u043B\u044C\u0437\u0443\u044E\u0442\u0441\u044F `println`, `printf`, `pr`,\
  \ \u0438\u043B\u0438 `prn`."
title: "\u0412\u044B\u0432\u043E\u0434 \u043E\u0442\u043B\u0430\u0434\u043E\u0447\u043D\
  \u043E\u0439 \u0438\u043D\u0444\u043E\u0440\u043C\u0430\u0446\u0438\u0438"
weight: 33
---

## Как это сделать:
В Clojure для вывода отладочной информации часто используются `println`, `printf`, `pr`, или `prn`. Вот как вы можете добавить в код отладочные печати:

```Clojure
(defn add-and-print [a b]
  (println "Сложение:" a "и" b) ; Выводит операцию
  (let [result (+ a b)]
    (println "Результат:" result)  ; Выводит результат
    result))                       ; Возвращает результат

(add-and-print 3 4)
```
Пример вывода:
```
Сложение: 3 и 4
Результат: 7
```

Или, для отладки значений в середине макроса потоковой передачи:

```Clojure
(require '[clojure.pprint :refer [pprint]])

(-> 3
    (+ 5)
    (pprint)             ; Выводит промежуточный результат
    (* 2))
```
Пример вывода:
```
8
```

## Подробнее:
Отладка с помощью вывода имеет долгую историю, вероятно, старую как и само программирование. Это просто: вы вставляете операторы вывода там, где подозреваете наличие проблем, запускаете код и смотрите на вывод.

Функции Clojure для вывода отладочной информации очень похожи на те, что используются в других языках Lisp, но с обычным функциональным оттенком. `println` и `prn` отличаются тем, что последний пишет данные так, чтобы их мог прочитать интерпретатор Clojure. `pprint` (красивый вывод) из `clojure.pprint` можно использовать, когда вы хотите более приятный формат.

Специфичный для Clojure инструмент для отладки - `tap>`. Представленный в Clojure 1.10, он позволяет осуществлять неблокирующие "подключения" к выполняющемуся коду, без необходимости загромождать ваш код операторами вывода.

Для более крупных или сложных проектов стоит рассмотреть библиотеку логирования, как `clojure.tools.logging` или `timbre`.

## Смотрите также:
- [Репозиторий `clojure.tools.logging` на GitHub](https://github.com/clojure/tools.logging)
- [Библиотека логирования Timbre](https://github.com/ptaoussanis/timbre) на GitHub
- [Документация на `clojure.pprint`](https://clojuredocs.org/clojure.pprint/pprint) на ClojureDocs
