---
title:                "Отправка HTTP-запроса"
date:                  2024-01-29T00:02:21.061016-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отправка HTTP-запроса"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/haskell/sending-an-http-request.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Отправка HTTP-запроса - это запрос к веб-серверу о предоставлении данных или выполнении действия. Программисты делают это для взаимодействия с API, получения веб-контента или коммуникации между сервисами.

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
