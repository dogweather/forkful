---
title:                "Журналирование"
date:                  2024-01-29T00:00:12.036060-07:00
model:                 gpt-4-0125-preview
simple_title:         "Журналирование"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/haskell/logging.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Логгирование в программировании — это, по сути, оставление следа в виде записанных событий или сообщений, которые могут быть использованы для отслеживания действий вашего приложения в любой данный момент. Программисты делают это для отладки проблем, мониторинга производительности системы и аудита поведения по причинам безопасности и соответствия нормативным требованиям.

## Как:

В Haskell логгирование может быть реализовано с помощью библиотек, таких как `monad-logger` или `hslogger`. Вот короткий пример с использованием `monad-logger`:

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Logger
import Control.Monad.IO.Class (liftIO)

logExample :: LoggingT IO ()
logExample = do
    logInfoN "Запуск приложения..."
    liftIO $ putStrLn "Выполнение критически важной работы..."
    logErrorN "Ой! Что-то пошло не так."

main :: IO ()
main = runStdoutLoggingT logExample

{- Пример вывода
[Info] Запуск приложения...
Выполнение критически важной работы...
[Error] Ой! Что-то пошло не так.
-}
```

Этот простой пример демонстрирует, как вы можете расставлять заявления о логгировании по всему своему коду, чтобы получить представление о том, что происходит во время выполнения. `logInfoN` и `logErrorN` используются для записи информационных и сообщений об ошибках соответственно.

## Подробнее:

Логгирование прошло долгий путь от простых команд печати до сложных систем логгирования. Исторически логи были просто текстовыми выводами в консоль или файл, но теперь они включают структурированные данные, которые могут быть проанализированы различными инструментами.

В Haskell логгирование может быть выполнено в чисто функциональном стиле, который включает явную передачу действий логгирования или использование монадических контекстов для нечистот, где логгеры неявно протягиваются через вычисления.

Например, библиотека `hslogger` более традиционная и изменяемая по сравнению с `monad-logger`. `monad-logger` предлагает интеграцию с монадическим стеком и обеспечивает большую гибкость в плане форматирования вывода и управления. Обе библиотеки позволяют устанавливать уровни логгирования, которые помогают фильтровать сообщения логов на основе их важности. Уровни логгирования включают debug, info, notice, warning, error, critical, alert и emergency.

Подход Haskell к логгированию часто согласуется с его акцентом на типовую безопасность и чистоту. Логи могут быть обработаны таким образом, что даже если логгирование потерпит неудачу, это не приведет к сбою основного приложения из-за надежных возможностей обработки ошибок Haskell.

## Смотрите также:
- [Документация `monad-logger` на Hackage](https://hackage.haskell.org/package/monad-logger)
- [Пакет `hslogger` на Hackage](https://hackage.haskell.org/package/hslogger)
- [Real World Haskell, Глава 19, обработка ошибок](http://book.realworldhaskell.org/read/error-handling.html)
- [Фасад логгирования для Haskell (log-base)](https://hackage.haskell.org/package/log-base)