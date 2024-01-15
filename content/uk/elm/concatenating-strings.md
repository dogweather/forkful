---
title:                "З'єднання рядків"
html_title:           "Elm: З'єднання рядків"
simple_title:         "З'єднання рядків"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Чому

У звичайних мовах програмування, потрібно додатково використовувати оператор `+` для з'єднання двох рядків в один. Однак, у Elm, є більш простий спосіб - використання функції `String.concat`, яка дозволяє об'єднувати будь-яку кількість рядків без додаткових символів або функцій. Тому, використання `String.concat` є ефективний і зручний спосіб для об'єднання рядків у Elm.

## Як

```Elm
let name = "John"
let greeting = "Hello"

String.concat [greeting, " ", name] -- output: "Hello John"
```

Функція `String.concat` має тип `List String -> String`, що означає, що приймає список рядків і повертає один об'єднаний рядок. У прикладі вище, ми задаємо список `["Hello", " ", "John"]` для функції `String.concat`, яка повертає рядок "Hello John".

Також, `String.concat` може бути використаний для об'єднання більшої кількості рядків:

```Elm
String.concat ["I", " ", "love", " ", "Elm"] -- output: "I love Elm"
```

Якщо потрібно об'єднати рядки зі значеннями інших типів даних, їх потрібно перетворити на рядки за допомогою функції `toString`:

```Elm
let num = 5

String.concat ["I have ", toString num, " apples"] -- output: "I have 5 apples"
```

## Deep Dive

`String.concat` внутрішньо використовує функцію `String.join`, яка дозволяє об'єднувати рядки з використанням роздільника. Наприклад:

```Elm
String.join " " ["I", "love", "Elm"] -- output: "I love Elm"
```

Також, `String.concat` дозволяє об'єднувати рядки з використанням множника, який повторить рядок певну кількість разів. Наприклад:

```Elm
String.concat (List.repeat 5 "Elm") -- output: "ElmElmElmElmElm"
```

Це корисна функціональність, яка дозволяє створити повторюючийся рядок з одним заданим значенням.

## Дивіться також
- [Офіційна документація Elm по функції String.concat](https://package.elm-lang.org/packages/elm/core/latest/String#concat)
- [Основні функції рядків у Elm](https://elmprogramming.com/basic-string-functions.html)