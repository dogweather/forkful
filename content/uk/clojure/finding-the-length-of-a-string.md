---
title:                "Знаходження довжини рядка"
html_title:           "Arduino: Знаходження довжини рядка"
simple_title:         "Знаходження довжини рядка"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Що і навіщо?

Визначення довжини рядка включає в себе визначення кількості символів у даному рядку. Програмісти роблять це, щоб маніпулювати даними, перевіряти вхід на правильність або зрозуміти структуру даних рядків.

## Як це зробити:

Clojure надає простй і прямолінійний метод для визначення довжини рядка - використовуючи `count` функцію. Також, в Clojure symbols і strings, розглядаються як послідовності.

```Clojure
(def my-string "Hello, Ukraine!")
(count my-string)
```

Як результат виконання коду, ми отримаємо 16, що є довжиною рядка 'Hello, Ukraine!'.

## Поглиблений огляд:

### Історичний контекст:
Функція count була частиною Clojure з самого початку як базова операція на колекції. Її ясність та універсальність роблять це гнучким інструментом, чи то вам потрібно рахувати елементи в колекції, чи символи в рядку.

### Альтернативи:
Іноді, ви можете бачити використання `(.length my-string)` замість `count`. Так, це працює, але зазвичай це менш ідіоматичний стиль в Clojure.

### Деталі реалізації:
В Clojure, рядки і символи вважаються послідовностями. `count` просто проходить через цю послідовність, рахуючи кожен елемент. 

## Дивіться також:

1. Офіційна документація по string - [https://clojuredocs.org/clojure.core/string] (https://clojuredocs.org/clojure.core/string)
2. Офіційна документація по `count` - [https://clojuredocs.org/clojure.core/count] (https://clojuredocs.org/clojure.core/count)
3. Блог про обробку рядків в Clojure - [https://www.braveclojure.com/core-functions-in-depth/#Strings] (https://www.braveclojure.com/core-functions-in-depth/#Strings)