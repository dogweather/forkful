---
date: 2024-01-20 17:52:23.581514-07:00
description: "\u042F\u043A \u0446\u0435 \u0440\u043E\u0431\u0438\u0442\u0438: \u0412\
  \u0438\u0432\u0456\u0434."
lastmod: '2024-04-05T21:53:49.359330-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u0412\u0438\u0432\u0435\u0434\u0435\u043D\u043D\u044F \u043D\u0430\u043B\u0430\
  \u0433\u043E\u0434\u0436\u0443\u0432\u0430\u043B\u044C\u043D\u043E\u0457 \u0456\u043D\
  \u0444\u043E\u0440\u043C\u0430\u0446\u0456\u0457"
weight: 33
---

## Як це робити:
```Elm
import Html exposing (text)
import Debug

main =
  let
    valueToInspect = "Hello, Elm!"
  in
  text (Debug.toString valueToInspect)
```
Вивід:
```
"Hello, Elm!"
```
За допомогою `Debug.toString` можна перетворити дані в рядок і вивести їх.

## Детальніше:
У Elm, відлагоджувальний вивід не призначений для використання в продакшн-коді; це інструмент для розробки. Починаючи з версії 0.19, Elm вилучає `Debug` модулі з продакшн-білдів. Історично, такий підхід допомагав запобігти випадковому розкриттю тестових даних та способів роботи внутрішньої логіки програми користувачам. Альтернативою є використання інших інструментів, як-то elm-monitor, для більш глибокого відлагодження. Реалізація друку в Elm базується на чистій функціональній природі мови, тому `Debug.log` які вносять побічні ефекти, використовуються обережно та свідомо.

## Дивись також:
- [Офіційна документація Elm про відлагоджувальний вивід](https://package.elm-lang.org/packages/elm/browser/latest/Browser#sandbox)
- [Робота з Debug.toText у Elm](https://package.elm-lang.org/packages/elm/core/latest/Debug#toString)
- [Курс Elm для початківців](https://korban.net/elm/)
- [elm-monitor для відлагодження Elm програм](https://github.com/layflags/elm-monitor)
