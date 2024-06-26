---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:55:38.304696-07:00
description: "\u041A\u0430\u043A: \u0412 Elm \u0435\u0441\u0442\u044C \u0430\u043A\
  \u043A\u0443\u0440\u0430\u0442\u043D\u044B\u0439 \u043E\u043F\u0435\u0440\u0430\u0442\
  \u043E\u0440 `(++)`, \u043A\u043E\u0442\u043E\u0440\u044B\u0439 \u043F\u0440\u0438\
  \u0445\u043E\u0434\u0438\u0442 \u043D\u0430 \u043F\u043E\u043C\u043E\u0449\u044C\
  ."
lastmod: '2024-03-13T22:44:44.886414-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Elm \u0435\u0441\u0442\u044C \u0430\u043A\u043A\u0443\u0440\u0430\
  \u0442\u043D\u044B\u0439 \u043E\u043F\u0435\u0440\u0430\u0442\u043E\u0440 `(++)`,\
  \ \u043A\u043E\u0442\u043E\u0440\u044B\u0439 \u043F\u0440\u0438\u0445\u043E\u0434\
  \u0438\u0442 \u043D\u0430 \u043F\u043E\u043C\u043E\u0449\u044C."
title: "\u0421\u043A\u043B\u0435\u0438\u0432\u0430\u043D\u0438\u0435 \u0441\u0442\u0440\
  \u043E\u043A"
weight: 3
---

## Как:
В Elm есть аккуратный оператор `(++)`, который приходит на помощь:

```Elm
greeting : String
greeting =
    "Привет, " ++ "мир!"

-- "Привет, мир!"
```

Но иногда у вас есть куча частей. Не бойтесь, `++` можно использовать по цепочке:

```Elm
fullName : String
fullName =
    "Elm" ++ " " ++ "Lang"

-- "Elm Lang"
```

А для списков строк `String.join` - ваш друг:

```Elm
words : List String
words =
    ["Присоединяйтесь", "к", "клубу", "Elm"]

sentence : String
sentence =
    String.join " " words

-- "Присоединяйтесь к клубу Elm"
```

## Погружение
В старые времена часто приходилось соединять строки с помощью сложных функций в других языках. В Elm это всегда было легко благодаря оператору `(++)`. Если вам действительно нужно много соединять, эффективность может оказаться важной; использование `(++)` с длинными строками может быть медленнее, потому что Elm должен пройти через всю строку слева от `(++)` каждый раз.

В некоторых языках также есть "интерполяция", но Elm не поддерживает интерполяцию строк. Но не стоит беспокоиться, `(++)` и `String.join` нас выручают.

Внутри, когда Elm соединяет строки, он старается быть умным об этом, часто используя оптимизированные операции JavaScript, на который в конце концов компилируется Elm. Так что даже если `(++)` может показаться простым, за кулисами происходит что-то умное, чтобы все работало быстро.

## Смотрите также
- Официальная документация Elm о строках: https://package.elm-lang.org/packages/elm/core/latest/String
- Руководство по Elm, где вы можете узнать больше о строках: https://guide.elm-lang.org/strings/
