---
title:                "Заголовок статті: Робота з регістром рядка"
html_title:           "Elm: Заголовок статті: Робота з регістром рядка"
simple_title:         "Заголовок статті: Робота з регістром рядка"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Чому

В цій статті ми детальніше розглянемо, як використовувати Elm для капіталізації рядків у нашому коді. Якщо ви пишете програми, які потребують великих літер у певних частинах рядків, ця функціональність буде корисною для вас.

## Як зробити

Для початку, нам потрібно імпортувати бібліотеку `String` із стандартної бібліотеки Elm. Потім, ми можемо скористатися функцією `toUpper` для перетворення потрібних літер у нашому рядку у великі.

```Elm
import String

String.toUpper "hello world" -- повертає "HELLO WORLD"
```

Якщо нам потрібно капіталізувати лише першу літеру рядка, ми можемо застосувати функції `String.capitalize` або `String.toUpper <@.> String.left`:

```Elm
String.capitalize "elm code" -- повертає "Elm code"

"elm code" |> String.toUpper <@.> String.left 1 -- повертає "Elm code"
```

## Глибший розгляд

Якщо ви хочете дізнатися більше про те, як працюють ці функції, ви можете використати функцію `Debug.log` для виводу проміжних результатів. Наприклад:

```Elm
import Debug

String.toUpper "hello" |> Debug.log "uppercase" -- побачимо "uppercase: HELLO"
```

Функція `Debug.log` допомагає нам на кожному кроці подивитися, що змінюється у нашому рядку під час виконання функцій.

## Дивіться також

- [Документація Elm про модуль `String`](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Стаття про перетворення рядків великі і малі літери в Elm](https://whatiselm.com/blog/capitalize-and-lowercase-in-elm)