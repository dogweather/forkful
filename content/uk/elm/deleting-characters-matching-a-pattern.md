---
title:                "Видалення символів, відповідних шаблону"
html_title:           "Elm: Видалення символів, відповідних шаблону"
simple_title:         "Видалення символів, відповідних шаблону"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Чому

Це може бути корисно, якщо вам потрібно оновити або очистити вхідні дані, видаляючи певні символи або шаблони.

## Як це зробити

Видалення символів за певним шаблоном можна зробити за допомогою функції `String.filter`. Для прикладу, якщо ми хочемо видалити всі числа зі стрічки, ми можемо використовувати такий код:

```Elm
import String exposing (filter)

toRemove = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
input = "Hello123World"

filteredOutput = filter (\char -> not (List.member char toRemove)) input

-- Результат: "HelloWorld"
```

Ми імпортуємо модуль `String` для доступу до функції `filter`. Потім ми створюємо список символів, які потрібно видалити, і вхідну стрічку. У функції `filter` ми передаємо анонімну функцію, яка перевіряє, чи не містить символ список `toRemove`, якщо так, то вона його не включає в вихідну стрічку. На останок, ми отримуємо вихідну стрічку без будь-яких чисел.

## Глибоке дослідження

У функції `filter` може бути використана будь-яка функція, яка повертає `Bool`. Тому, ви можете використовувати цю функцію для видалення будь-яких символів, та навіть для складніших шаблонів. Наприклад, ви можете видалити всі символи, що не є літерами, використовуючи функцію `Char.isAlpha`.

```Elm
import String exposing (filter)
import Char exposing (isAlpha)

input = "Hello123World"

filteredOutput = filter isAlpha input

-- Результат: "HelloWorld"
```

Також, ви можете використовувати функції `String.map` та `String.foldl` для більш складних операцій зі стрічками.

## Дивіться також

- [Функція `String.filter` у документації Elm](https://package.elm-lang.org/packages/elm/core/latest/String#filter)
- [Робота зі стрічками у Elm](https://guide.elm-lang.org/strings/)