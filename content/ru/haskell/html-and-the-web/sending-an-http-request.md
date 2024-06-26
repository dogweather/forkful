---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:21.061016-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u041F\u0435\u0440\u0435\u0439\u0434\u0435\u043C \u043A \u0438\u043D\
  \u0442\u0435\u0440\u0435\u0441\u043D\u043E\u043C\u0443. \u0412\u0430\u043C \u043F\
  \u043E\u043D\u0430\u0434\u043E\u0431\u044F\u0442\u0441\u044F \u043F\u0430\u043A\u0435\
  \u0442\u044B `http-client` \u0438 `http-client-tls`. \u041D\u0430\u0441\u0442\u0440\
  \u043E\u0439\u0442\u0435 \u0432\u0430\u0448 \u0441\u0442\u0435\u043A \u0438 \u0434\
  \u043E\u0431\u0430\u0432\u044C\u0442\u0435 \u0438\u0445 \u0432 \u0432\u0430\u0448\
  \ \u0444\u0430\u0439\u043B\u2026"
lastmod: '2024-03-13T22:44:45.128002-06:00'
model: gpt-4-0125-preview
summary: "\u041F\u0435\u0440\u0435\u0439\u0434\u0435\u043C \u043A \u0438\u043D\u0442\
  \u0435\u0440\u0435\u0441\u043D\u043E\u043C\u0443."
title: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\u0440\
  \u043E\u0441\u0430"
weight: 44
---

## Как это сделать:
Перейдем к интересному. Вам понадобятся пакеты `http-client` и `http-client-tls`. Настройте ваш стек и добавьте их в ваш файл `package.yaml` или `.cabal`. Затем выполните команду `stack build` или другие соответствующие команды для их загрузки.

Вот простой GET запрос:

```Haskell
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.ByteString.Lazy.Char8 as L8

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest "http://httpbin.org/get"
    response <- httpLbs request manager
    L8.putStrLn $ responseBody response
```

Это выведет на печать JSON, полученный от `httpbin.org`.

## Погружение в детали
Раньше, запросы HTTP в Haskell были менее очевидными, но библиотеки, такие как `http-client`, упростили этот процесс.

Альтернативы? Конечно. Есть `wreq`, `req` и другие, часто с синтаксическим сахаром или дополнительными функциями. Но `http-client` это как тот надежный швейцарский нож в вашем ящике – он всегда выполняет работу.

Под капотом `http-client` использует `Manager` для управления соединениями. Это эффективно и позволяет использовать сокеты повторно. Можно его настроить, но по умолчанию это тоже хороший старт.

## Смотрите также
Чтобы расширить ваш арсенал, посмотрите следующее:

- [Пакет `http-client`](https://www.stackage.org/package/http-client)
- [Пакет `wreq` для более современного подхода](https://www.stackage.org/package/wreq)
- [Hackage для библиотек Haskell](https://hackage.haskell.org/)
