---
title:                "Виведення налагоджувальної інформації"
aliases:
- uk/elm/printing-debug-output.md
date:                  2024-01-20T17:52:23.581514-07:00
model:                 gpt-4-1106-preview
simple_title:         "Виведення налагоджувальної інформації"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Що це таке та навіщо?
Друкувати відлагоджувальний вивід – це спосіб вивести на екран змінні та дані для перевірки та аналізу коду. Програмісти роблять це, щоб легше знаходити та виправляти помилки.

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
