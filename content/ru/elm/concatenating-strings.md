---
title:                "Склеивание строк"
date:                  2024-01-28T23:55:38.304696-07:00
model:                 gpt-4-0125-preview
simple_title:         "Склеивание строк"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/elm/concatenating-strings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?
Конкатенация строк означает склеивание двух или более фрагментов текста вместе. Это так же основно и необходимо, как использование скотча, позволяя вам на лету создавать новые строки для отображения сообщений, создания шаблонов и многого другого.

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