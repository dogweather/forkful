---
title:                "Elm: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Чому

Можливо, ви стикнулись з проблемою, коли потрібно було отримати вміст веб-сторінки не використовуючи браузер, але не знали як це зробити. Мабуть, ви також хочете науково вивчати HTML код сторінки або перевіряти її наявність. У цьому випадку, вам може знадобитися Elm для завантаження змісту веб-сторінки.

## Як це зробити

Використовуючи Elm, можна легко зробити запит на веб-сторінку та отримати її вміст. Нижче наведено приклад коду, який використовує модуль `Http` для цього:

```Elm
import Http exposing (..)
import String exposing (..)

type Msg = 
    Success String 
    | Failure Http.Error

url : String 
url = "https://example.com"

fetchPage : Cmd Msg 
fetchPage = 
    send Success Failure <| getString url

view : String -> Html Msg
view content = 
    text content

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of 
        Success content -> (model, Cmd.none)
        Failure error -> (model, Cmd.none)

main : Program Never Model Msg 
main = 
    program 
        { init = (Model, fetchPage)
        , view = view 
        , update = update 
        , subscriptions = always Sub.none
        }
```

Після запуску цього коду, зміст веб-сторінки буде виведено на екран у вигляді звичайного тексту. Ви також можете додати функціональність для обробки результатів запиту та додаткових дій за допомогою типу `Msg` та функції `update`.

## Глибше вдивимося

Звертаючись до веб-сторінки за допомогою Elm, всі запити відбуваються асинхронно, тому не блокується виконання решти програми. Крім того, модуль `Http` надає багато різноманітних функцій для роботи з веб-сторінками, таких як `post`, `put` та `delete`. Для більш детальної інформації про цей модуль та інші можливості Elm, вам може стати корисним офіційний сайт та спільнота користувачів.

## Дивіться також

Вашім наступним кроком може бути вивчення інших можливостей Elm та Task Сhannel для взаємодії з веб-сторінками за допомогою команд та платформи Node.js. Ось кілька посилань, які можуть вам пригодитися:

- Офіційний сайт Elm: https://elm-lang.org/
- Аналіз HTML в Elm: https://package.elm-lang.org/packages/elm/html/latest/
- Створення web app за допомогою Elm та Node.js: https://elm-lang.org/0.19.0/init