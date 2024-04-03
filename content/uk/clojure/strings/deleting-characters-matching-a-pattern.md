---
date: 2024-01-20 17:42:15.025563-07:00
description: "\u0412\u0438\u0434\u0430\u043B\u0435\u043D\u043D\u044F \u0441\u0438\u043C\
  \u0432\u043E\u043B\u0456\u0432, \u044F\u043A\u0456 \u0432\u0456\u0434\u043F\u043E\
  \u0432\u0456\u0434\u0430\u044E\u0442\u044C \u043F\u0430\u0442\u0442\u0435\u0440\u043D\
  \u0443, - \u0446\u0435 \u043F\u0440\u043E\u0446\u0435\u0441 \u0444\u0456\u043B\u044C\
  \u0442\u0440\u0430\u0446\u0456\u0457 \u0440\u044F\u0434\u043A\u0430 \u0437\u0430\
  \ \u0434\u043E\u043F\u043E\u043C\u043E\u0433\u043E\u044E \u0432\u0438\u0437\u043D\
  \u0430\u0447\u0435\u043D\u043E\u0433\u043E \u0448\u0430\u0431\u043B\u043E\u043D\u0443\
  . \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0440\u043E\
  \u0431\u043B\u044F\u0442\u044C \u0446\u0435 \u0434\u043B\u044F \u0447\u0438\u0441\
  \u0442\u043A\u0438 \u0434\u0430\u043D\u0438\u0445,\u2026"
lastmod: '2024-03-13T22:44:48.626199-06:00'
model: gpt-4-1106-preview
summary: "\u0412\u0438\u0434\u0430\u043B\u0435\u043D\u043D\u044F \u0441\u0438\u043C\
  \u0432\u043E\u043B\u0456\u0432, \u044F\u043A\u0456 \u0432\u0456\u0434\u043F\u043E\
  \u0432\u0456\u0434\u0430\u044E\u0442\u044C \u043F\u0430\u0442\u0442\u0435\u0440\u043D\
  \u0443, - \u0446\u0435 \u043F\u0440\u043E\u0446\u0435\u0441 \u0444\u0456\u043B\u044C\
  \u0442\u0440\u0430\u0446\u0456\u0457 \u0440\u044F\u0434\u043A\u0430 \u0437\u0430\
  \ \u0434\u043E\u043F\u043E\u043C\u043E\u0433\u043E\u044E \u0432\u0438\u0437\u043D\
  \u0430\u0447\u0435\u043D\u043E\u0433\u043E \u0448\u0430\u0431\u043B\u043E\u043D\u0443\
  ."
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
