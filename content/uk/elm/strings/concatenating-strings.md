---
aliases:
- /uk/elm/concatenating-strings/
date: 2024-01-20 17:34:34.976016-07:00
description: (
lastmod: 2024-02-18 23:09:00.178589
model: gpt-4-1106-preview
summary: (
title: "\u041E\u0431'\u0454\u0434\u043D\u0430\u043D\u043D\u044F \u0440\u044F\u0434\
  \u043A\u0456\u0432"
---

{{< edit_this_page >}}

## What & Why?
(## Що і Чому?)

Concatenating strings is about sticking two or more pieces of text together. We do it to build new strings from existing ones, like names or messages.

(З'єднання рядків полягає в склеюванні двох або більше шматків тексту. Ми робимо це, щоб створити нові рядки з існуючих, як от імена чи повідомлення.)

## How to:
(## Як це зробити:)

```Elm
greeting : String
greeting = "Привіт, "

name : String
name = "Андрію!"

fullGreeting : String
fullGreeting = greeting ++ name

-- Output: "Привіт, Андрію!"
```

(Сповнене привітання: "Привіт, Андрію!")

## Deep Dive
(## Поглиблений Розгляд)

Long ago, concatenating strings could be costly because of memory manipulations. In Elm, `++` is used for concatenation, and it's handled efficiently under the hood. Alternatives like StringBuilder in other languages are not necessary in Elm because of its functional nature and immutable strings. Those features guarantee that string manipulations don't cause side-effects or unnecessary performance hits.

(Давно, з'єднування рядків могло бути вартісним через маніпуляції з пам'яттю. В Elm, для з'єднання використовують `++`, і це ефективно обробляється під капотом. Альтернативи на зразок StringBuilder у інших мовах не потрібні в Elm через його функціональну сутність та незмінні рядки. Ці особливості гарантують, що маніпуляції з рядками не призводять до побічних ефектів чи непотрібних ударів по продуктивності.)

## See Also
(## Дивіться Також)

- Elm Official Documentation on String: https://package.elm-lang.org/packages/elm/core/latest/String
- Elm Strings Guide: https://elmprogramming.com/strings.html
- Elm Discourse for Community Support: https://discourse.elm-lang.org/

(Офіційна Документація Elm по Рядках: https://package.elm-lang.org/packages/elm/core/latest/String
Посібник Elm по Рядкам: https://elmprogramming.com/strings.html
Elm Discourse для підтримки спільноти: https://discourse.elm-lang.org/)
