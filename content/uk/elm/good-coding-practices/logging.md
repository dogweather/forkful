---
title:                "Логування"
aliases: - /uk/elm/logging.md
date:                  2024-01-26T01:03:59.572779-07:00
model:                 gpt-4-1106-preview
simple_title:         "Логування"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/logging.md"
---

{{< edit_this_page >}}

## Що і чому?
Логування - це по суті процес запису подій та виводів даних з програмного забезпечення під час його роботи, подумайте про це як про щоденник програми. Програмісти використовують логування, щоб стежити за тим, що відбувається під капотом - це незамінно для відладки проблем, моніторингу поведінки системи в реальному часі та аналізу минулої активності з метою оптимізації продуктивності або аудиту.

## Як:
Архітектура Elm не підтримує побічні ефекти, такі як логування, "з коробки" — ви обробляєте їх за допомогою команд, які є частиною архітектури вашого додатка. Для навчальних цілей, давайте подивимося, як ви могли б симулювати логування, відправляючи повідомлення у JavaScript через порти.

Спочатку ви визначите порт-модуль:

```Elm
port module Logger exposing (..)

-- Визначити порт для надсилання логів у JavaScript
port log : String -> Cmd msg
```

У вашому `Main.elm` ви б використовували порт `log` для відправки повідомлення про лог:

```Elm
import Logger exposing (log)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        AnEvent ->
            -- тут деякі оновлення вашої моделі
            ( updatedModel, log "AnEvent occurred." )

        AnotherEvent ->
            -- тут інші оновлення моделі
            ( anotherUpdatedModel, log "AnotherEvent occurred." )
```

На стороні JavaScript ви б підписалися на порт `log` для обробки вхідних повідомлень про логи:

```JavaScript
var app = Elm.Main.init({ /* ... */ });

app.ports.log.subscribe(function(message) {
    console.log(message);
});
```

Приклад виводу в консолі JavaScript буде таким:

```
AnEvent occurred.
AnotherEvent occurred.
```

## Глибше занурення
Традиційно, в мовах програмування, таких як Python чи Java, логування виконується за допомогою бібліотеки логування, яка надає прямий API для логування повідомлень на різних рівнях, таких як debug, info, warning, error і critical.

Elm, з його акцентом на чистоту та незмінність, не забезпечує такого прямого логування, оскільки будь-який вид вводу-виводу або побічний ефект керується окремо через архітектуру Elm.

Коли вам потрібно повнофункціональне логування в Elm, ви зазвичай покладаєтеся на зовнішні інструменти JavaScript. Порти, як було показано вище, є мостом до цих інструментів. Модуль Debug - ще один варіант, але він призначений тільки для використання під час розробки, а не для логування в продакшені.

Крім портів, програмісти часто використовують повідомлення компілятора Elm та засоби відладки в робочому часі, такі як `Debug.log`, які ви можете вставити у ваш код для відстеження значень. Він обгортає вираз і записує його вивід в консоль так:

```Elm
view model =
    Debug.log "Model Debug" model
    -- тут ваш код відображення
```

Однак це теж не призначено для використання у продакшені. Інструменти на кшталт elm-logger забезпечують деякі абстракції над портами для логування, хоча ці також призначені більше для розробки, а не для продакшену.

## Дивіться також
- Elm порти: https://guide.elm-lang.org/interop/ports.html
- Elm `Debug`: https://package.elm-lang.org/packages/elm/core/latest/Debug
- Обговорення Elm та логування: https://discourse.elm-lang.org/t/elm-and-logging/546
- JavaScript Console API: https://developer.mozilla.org/en-US/docs/Web/API/Console
- пакет elm-logger: https://package.elm-lang.org/packages/arkgil/elm-logger/latest/
