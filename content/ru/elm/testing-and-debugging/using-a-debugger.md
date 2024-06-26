---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:17.117832-07:00
description: "\u041A\u0430\u043A: \u0412 Elm \u043D\u0435\u0442 \u0432\u0441\u0442\
  \u0440\u043E\u0435\u043D\u043D\u043E\u0433\u043E \u043E\u0442\u043B\u0430\u0434\u0447\
  \u0438\u043A\u0430 \u0432 \u0442\u0440\u0430\u0434\u0438\u0446\u0438\u043E\u043D\
  \u043D\u043E\u043C \u0441\u043C\u044B\u0441\u043B\u0435, \u043A\u0430\u043A, \u043D\
  \u0430\u043F\u0440\u0438\u043C\u0435\u0440, \u0432 JavaScript \u0441 \u0438\u043D\
  \u0441\u0442\u0440\u0443\u043C\u0435\u043D\u0442\u0430\u043C\u0438 \u0440\u0430\u0437\
  \u0440\u0430\u0431\u043E\u0442\u0447\u0438\u043A\u0430 \u0431\u0440\u0430\u0443\u0437\
  \u0435\u0440\u0430. \u0422\u0435\u043C \u043D\u0435 \u043C\u0435\u043D\u0435\u0435\
  , \u0441\u043E\u043E\u0431\u0449\u0435\u0441\u0442\u0432\u043E\u2026"
lastmod: '2024-03-13T22:44:44.909902-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Elm \u043D\u0435\u0442 \u0432\u0441\u0442\u0440\u043E\u0435\u043D\
  \u043D\u043E\u0433\u043E \u043E\u0442\u043B\u0430\u0434\u0447\u0438\u043A\u0430\
  \ \u0432 \u0442\u0440\u0430\u0434\u0438\u0446\u0438\u043E\u043D\u043D\u043E\u043C\
  \ \u0441\u043C\u044B\u0441\u043B\u0435, \u043A\u0430\u043A, \u043D\u0430\u043F\u0440\
  \u0438\u043C\u0435\u0440, \u0432 JavaScript \u0441 \u0438\u043D\u0441\u0442\u0440\
  \u0443\u043C\u0435\u043D\u0442\u0430\u043C\u0438 \u0440\u0430\u0437\u0440\u0430\u0431\
  \u043E\u0442\u0447\u0438\u043A\u0430 \u0431\u0440\u0430\u0443\u0437\u0435\u0440\u0430\
  ."
title: "\u0418\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\
  \ \u043E\u0442\u043B\u0430\u0434\u0447\u0438\u043A\u0430"
weight: 35
---

## Как:
В Elm нет встроенного отладчика в традиционном смысле, как, например, в JavaScript с инструментами разработчика браузера. Тем не менее, сообщество Elm создало инструменты, чтобы заполнить этот пробел. Вот как вы можете использовать `elm-debug-transformer` для отладки вашего приложения Elm:

```Elm
-- Установите elm-debug-transformer (пакет Node)

1. npm install -g elm-debug-transformer

-- Используйте elm-debug-transformer для запуска вашего приложения

2. elm-debug-transformer --port=8000 yourMainElmFile.elm 
```

Как только `elm-debug-transformer` запущен, он создает соединение WebSocket для логирования. Вы увидите отладочную информацию в консоли вашего браузера, где сможете осмотреть структуры данных вашей программы в заданных точках вашего приложения.

В версиях Elm 0.19 и позже функции модуля `Debug` такие, как `Debug.log` и `Debug.todo`, могут помочь вам отслеживать значения и намеренно помечать незавершенные части вашего кода. Вот как использовать Debug.log:

```Elm
import Debug

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( Debug.log "Увеличение" { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( Debug.log "Уменьшение" { model | count = model.count - 1 }, Cmd.none )
```

Вы увидите сообщения "Увеличение" или "Уменьшение" в консоли вашего браузера вместе с новым состоянием `model`.

## Глубокое погружение
Автор Elm, Эван Цзапликки, стремился создать язык, в котором распространенные баги были бы невозможны или легко обнаружимы. Эта философия объясняет, почему в ядре Elm нет традиционных функций отладки. Статический анализ и вывод типов в Elm существенно снижают количество ошибок времени выполнения, что уменьшает необходимость в сложной отладке во время выполнения. Историческими альтернативами были использование теперь устаревшего `elm-reactor`, который предлагал отладку с возможностью перемотки времени - способ перематывать назад и воспроизводить действия в вашем приложении.

Сегодня инструменты вроде `elm-debug-transformer` и использование модуля `Debug` Elm помогают преодолеть этот пробел. Хотя модуль `Debug` предназначен для использования только во время разработки и должен быть удален перед сборкой для продакшена, он является неоценимым инструментом для точной идентификации и логирования изменений состояния.

Имейте в виду, что традиционные техники отладки JavaScript, такие как точки останова или пошаговое выполнение, неприменимы в Elm из-за его архитектуры и обработки обновлений состояния в рантайме Elm. Elm поощряет вас структурировать вашу программу таким образом, чтобы поток данных был ясным и следовал строгим типам и гарантиям неизменяемости, минимизируя случаи, когда необходима отладка.

## Смотрите также
- Официальное руководство Elm по обработке исключений времени выполнения: https://guide.elm-lang.org/error_handling/
- Репозиторий GitHub `elm-debug-transformer`: https://github.com/kraklin/elm-debug-transformer
- Ветка на форуме Elm, обсуждающая стратегии отладки: https://discourse.elm-lang.org/c/show-and-tell/debugging
- Документация модуля `Debug` Elm: https://package.elm-lang.org/packages/elm/core/latest/Debug
