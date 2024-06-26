---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:00:12.036060-07:00
description: "\u041A\u0430\u043A: \u0412 Haskell \u043B\u043E\u0433\u0433\u0438\u0440\
  \u043E\u0432\u0430\u043D\u0438\u0435 \u043C\u043E\u0436\u0435\u0442 \u0431\u044B\
  \u0442\u044C \u0440\u0435\u0430\u043B\u0438\u0437\u043E\u0432\u0430\u043D\u043E\
  \ \u0441 \u043F\u043E\u043C\u043E\u0449\u044C\u044E \u0431\u0438\u0431\u043B\u0438\
  \u043E\u0442\u0435\u043A, \u0442\u0430\u043A\u0438\u0445 \u043A\u0430\u043A `monad-logger`\
  \ \u0438\u043B\u0438 `hslogger`. \u0412\u043E\u0442 \u043A\u043E\u0440\u043E\u0442\
  \u043A\u0438\u0439 \u043F\u0440\u0438\u043C\u0435\u0440 \u0441 \u0438\u0441\u043F\
  \u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\u043C\u2026"
lastmod: '2024-03-13T22:44:45.145728-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Haskell \u043B\u043E\u0433\u0433\u0438\u0440\u043E\u0432\u0430\u043D\
  \u0438\u0435 \u043C\u043E\u0436\u0435\u0442 \u0431\u044B\u0442\u044C \u0440\u0435\
  \u0430\u043B\u0438\u0437\u043E\u0432\u0430\u043D\u043E \u0441 \u043F\u043E\u043C\
  \u043E\u0449\u044C\u044E \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\
  , \u0442\u0430\u043A\u0438\u0445 \u043A\u0430\u043A `monad-logger` \u0438\u043B\u0438\
  \ `hslogger`."
title: "\u0416\u0443\u0440\u043D\u0430\u043B\u0438\u0440\u043E\u0432\u0430\u043D\u0438\
  \u0435"
weight: 17
---

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
