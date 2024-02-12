---
title:                "Использование отладчика"
date:                  2024-01-29T00:04:17.117832-07:00
model:                 gpt-4-0125-preview
simple_title:         "Использование отладчика"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/elm/using-a-debugger.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Отладка в Elm включает в себя выявление и устранение ошибок из вашего кода. Программисты делают это, чтобы убедиться, что их приложения работают правильно и чтобы улучшить качество кода. Сильная система типов в Elm отлавливает многие проблемы на этапе компиляции, но инструменты для отладки во время выполнения необходимы для выявления и исправления логических ошибок и неожиданных поведений.

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