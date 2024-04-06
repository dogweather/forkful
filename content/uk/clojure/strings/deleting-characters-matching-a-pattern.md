---
date: 2024-01-20 17:42:15.025563-07:00
description: "How to: - \u042F\u043A \u0446\u0435 \u0440\u043E\u0431\u0438\u0442\u0438\
  : ."
lastmod: '2024-04-05T21:53:48.883747-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u0412\u0438\u0434\u0430\u043B\u0435\u043D\u043D\u044F \u0441\u0438\u043C\u0432\
  \u043E\u043B\u0456\u0432 \u0437\u0430 \u0432\u0456\u0437\u0435\u0440\u0443\u043D\
  \u043A\u043E\u043C"
weight: 5
---

## How to: - Як це робити:
```Clojure
; Використовуємо функцію re-seq для пошуку відповідностей
(defn find-pattern [pattern str]
  (re-seq (re-pattern pattern) str))

; Видаляємо цифри з рядка
(defn remove-digits [str]
  (clojure.string/replace str #"\d+" ""))

; Демонстраційні виклики
(println (find-pattern "\\d+" "abc123xyz")) ; => (123)
(println (remove-digits "abc123xyz"))       ; => "abcxyz"
```

## Deep Dive - Поглиблений аналіз:
Видалення символів, яке відповідають паттерну, може бути реалізовано за допомогою регулярних виразів, що з'явились ще в 1950-х і з тих пір стали стандартним інструментом в текстовій обробці. У Clojure, функції `re-seq`, `re-find`, та `clojure.string/replace` використовують Java Pattern клас під капотом для роботи з регулярними виразами. Ви можете вибирати підходящий інструмент в залежності від потреби: шукати всі відповідності чи тільки першу, заміняти знайдене чи співставляти шаблони.

Альтернативи включають використання готових бібліотек для парсингу (наприклад, instaparse) або мовні вбудовані інструменти (такі, як `filter` і `remove`). Підібрати найкращий варіант залежить від задачі: складність паттерну, продуктивність великих даних, читаність коду.

## See Also - Дивіться також:
- [ClojureDocs clojure.string/replace](https://clojuredocs.org/clojure.string/replace)
- [Java Pattern documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/regex/Pattern.html)
- [instaparse on Clojars](https://clojars.org/instaparse)
- [Clojure from the ground up: regular expressions](https://aphyr.com/posts/305-clojure-from-the-ground-up-regular-expressions)
