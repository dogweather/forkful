---
date: 2024-01-20 17:55:57.667492-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : Elm \u0444\u043E\u043A\u0443\u0441\u0443\u0454\u0442\u044C\u0441\u044F \u043D\u0430\
  \ \u0431\u0435\u0437\u043F\u0435\u0447\u043D\u0438\u0445 \u0432\u0435\u0431-\u0434\
  \u043E\u0434\u0430\u0442\u043A\u0430\u0445, \u0442\u043E\u043C\u0443 \u043D\u0435\
  \u043C\u0430\u0454 \u0456\u043D\u0442\u0435\u0433\u0440\u043E\u0432\u0430\u043D\u043E\
  \u0433\u043E \u0441\u043F\u043E\u0441\u043E\u0431\u0443 \u0434\u043E\u0441\u0442\
  \u0443\u043F\u0443 \u0434\u043E \u0430\u0440\u0433\u0443\u043C\u0435\u043D\u0442\
  \u0456\u0432 \u043A\u043E\u043C\u0430\u043D\u0434\u043D\u043E\u0433\u043E \u0440\
  \u044F\u0434\u043A\u0430, \u044F\u043A \u0446\u0435 \u043C\u043E\u0436\u043B\u0438\
  \u0432\u043E \u0432 \u043C\u043E\u0432\u0430\u0445,\u2026"
lastmod: '2024-04-05T21:53:49.378147-06:00'
model: gpt-4-1106-preview
summary: "Elm \u0444\u043E\u043A\u0443\u0441\u0443\u0454\u0442\u044C\u0441\u044F \u043D\
  \u0430 \u0431\u0435\u0437\u043F\u0435\u0447\u043D\u0438\u0445 \u0432\u0435\u0431\
  -\u0434\u043E\u0434\u0430\u0442\u043A\u0430\u0445, \u0442\u043E\u043C\u0443 \u043D\
  \u0435\u043C\u0430\u0454 \u0456\u043D\u0442\u0435\u0433\u0440\u043E\u0432\u0430\u043D\
  \u043E\u0433\u043E \u0441\u043F\u043E\u0441\u043E\u0431\u0443 \u0434\u043E\u0441\
  \u0442\u0443\u043F\u0443 \u0434\u043E \u0430\u0440\u0433\u0443\u043C\u0435\u043D\
  \u0442\u0456\u0432 \u043A\u043E\u043C\u0430\u043D\u0434\u043D\u043E\u0433\u043E\
  \ \u0440\u044F\u0434\u043A\u0430, \u044F\u043A \u0446\u0435 \u043C\u043E\u0436\u043B\
  \u0438\u0432\u043E \u0432 \u043C\u043E\u0432\u0430\u0445, \u043E\u0440\u0456\u0454\
  \u043D\u0442\u043E\u0432\u0430\u043D\u0438\u0445 \u043D\u0430 \u0437\u0430\u0433\
  \u0430\u043B\u044C\u043D\u0456 \u0437\u0430\u0432\u0434\u0430\u043D\u043D\u044F."
title: "\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0430\u0440\u0433\u0443\u043C\u0435\
  \u043D\u0442\u0456\u0432 \u043A\u043E\u043C\u0430\u043D\u0434\u043D\u043E\u0433\u043E\
  \ \u0440\u044F\u0434\u043A\u0430"
weight: 23
---

## Як це зробити:
```Elm
-- Elm не має вбудованої підтримки для читання аргументів командного рядка.
-- Однак, ви можете використовувати JavaScript через порти для отримання цих даних.
-- Давайте налаштуємо простий приклад.

port module Main exposing (..)

import Json.Decode as Decode
import Html

-- Оголошуємо порт для отримання аргументів.
port cmdlineArgs : (String -> msg) -> Sub msg

-- Тип повідомлення для вашого прикладу.
type Msg
    = CmdArgs String

-- Основна програма.
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- Початковий стан.
init : (Model, Cmd Msg)
init =
    ("Ніяких аргументів ще не отримано", Cmd.none)

-- Передплати.
subscriptions : Model -> Sub Msg
subscriptions model =
    cmdlineArgs CmdArgs

-- Представлення.
view : Model -> Html.Html Msg
view model =
    Html.text model

-- Оновлення.
update : Msg -> Model -> (Model, Cmd Msg)
update (CmdArgs args) _ =
    (args, Cmd.none)

-- Модель.
type alias Model =
    String

-- Запуск цього коду буде потребувати JavaScript коду, щоб відправити аргументи через порти.
```

## В глибину
Elm фокусується на безпечних веб-додатках, тому немає інтегрованого способу доступу до аргументів командного рядка, як це можливо в мовах, орієнтованих на загальні завдання. Звичайно для читання аргументів командного рядка використовуються скриптові мови як Python або Node.js, а Elm використовується для роботи на боці клієнта і маніпуляції DOM. Коли вам потрібно працювати з командним рядком, можна застосовувати Elm через порти, що дозволяють двосторонню інтеграцію з JavaScript.

## Також гляньте
- [Elm Port Example](https://guide.elm-lang.org/interop/ports.html) - як працювати з портами.
- [JSON Decode](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode) - документація про декодування JSON у Elm.
- [Node.js Process Arguments](https://nodejs.org/docs/latest/api/process.html#process_process_argv) - для роботи з аргументами командного рядка в Node.js, якщо ви хочете інтегрувати з Elm через порти.
