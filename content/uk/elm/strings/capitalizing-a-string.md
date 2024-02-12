---
title:                "Зробити першу літеру рядка великою"
aliases: - /uk/elm/capitalizing-a-string.md
date:                  2024-02-03T19:05:38.870500-07:00
model:                 gpt-4-0125-preview
simple_title:         "Зробити першу літеру рядка великою"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та чому?

Перетворення рядка на великі літери включає зміну початкового символу заданого рядка на велику букву, залишаючи решту символів малими, часто для стандартизації форматування чи з міркувань читабельності. Програмісти часто виконують це завдання, щоб забезпечити послідовне представлення даних, особливо у користувацьких інтерфейсах або під час обробки та відображення вводу користувача.

## Як це зробити:

У Elm немає вбудованої функції спеціально для перетворення рядків на великі літери. Однак, ви можете легко досягти цього, використовуючи функції модулю `String`, такі як `toUpper`, `toLower`, `left` та `dropLeft`.

```elm
capitalize : String -> String
capitalize str =
    if String.isEmpty str then
        ""
    else
        String.toUpper (String.left 1 str) ++ String.toLower (String.dropLeft 1 str)

-- Приклад використання
main =
    String.toList "hello world" |> List.map capitalize |> String.join " "
    -- Вивід: "Hello World"
```

Для більш складних сценаріїв або якщо ви вважаєте за краще використовувати бібліотеку, яка прямо дозволяє перетворювати рядки на великі літери, ви можете розглянути пакет сторонніх розробників, такий як `elm-community/string-extra`. Однак, станом на мій останній оновлення, екосистема Elm спонукає виконувати такі завдання за допомогою вбудованих функцій, щоб мова та проєкти залишались мінімалістичними.

```elm
import String.Extra as StringExtra

-- У випадку, якщо в бібліотеці сторонних розробників є функція `capitalize`
capitalizeWithLibrary : String -> String
capitalizeWithLibrary str =
    StringExtra.capitalize str

-- Приклад використання з гіпотетичною функцією бібліотеки
main =
    "this is elm" |> capitalizeWithLibrary
    -- Гіпотетичний вивід: "This is elm"
```

Завжди перевіряйте репозиторій пакетів Elm для останніх і найбільш підходящих бібліотек для маніпулювання рядками, якщо ви шукаєте додатковий функціонал поза стандартною бібліотекою.
