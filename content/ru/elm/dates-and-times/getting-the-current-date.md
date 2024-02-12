---
title:                "Получение текущей даты"
aliases:
- /ru/elm/getting-the-current-date/
date:                  2024-01-28T23:58:26.359053-07:00
model:                 gpt-4-0125-preview
simple_title:         "Получение текущей даты"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/elm/getting-the-current-date.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и зачем?
Получение текущей даты в Elm означает извлечение текущей календарной даты из системы. Мы делаем это для того, чтобы ставить временные метки на события, планировать задачи или отслеживать продолжительность выполнения задач.

## Как:
Elm обрабатывает даты с помощью модуля `Time`. Вы получите текущее время в виде POSIX-метки времени, затем конвертируете его в дату.

```Elm
import Browser
import Task
import Time

type Msg = GetCurrentTime Time.Posix

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GetCurrentTime posixTime ->
            let
                -- Конвертация POSIX времени в запись даты
                date = Time.toDate posixTime
            in
            -- Здесь соответствующим образом обновите вашу модель
            ({ model | date = date }, Cmd.none)

-- Для инициации получения текущего времени
getCurrentTime : Cmd Msg
getCurrentTime =
    Task.perform GetCurrentTime Time.now

-- Пример вывода:
-- date { year = 2023, month = Mar, day = 26 }
```

## Погружение
В старых веб-языках получение даты - это код, состоящий из одной строки. Elm отличается. Он делает побочные эффекты, такие как получение текущего времени, явными через Архитектуру Elm. Это поощряет чистоту и поддерживаемость кода.

Альтернативы включают использование сторонних пакетов или обработку дат на вашем сервере и передачу их в Elm через флаги или порты.

С точки зрения реализации, `Time.now` в Elm получает время как POSIX-метку времени (миллисекунды с момента начала эпохи Unix). Это не зависит от часового пояса, и вы можете форматировать его по мере необходимости, используя функции из модуля `Time`.

## Смотрите также
- [Документация по времени в Elm](https://package.elm-lang.org/packages/elm/time/latest/)
- [Руководство Elm по командам и подпискам](https://guide.elm-lang.org/effects/)
