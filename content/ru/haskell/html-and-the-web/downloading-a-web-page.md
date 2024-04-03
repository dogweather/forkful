---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:57:29.220170-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0440\u0430\u0441\u0441\
  \u043C\u043E\u0442\u0440\u0438\u043C \u043F\u0440\u043E\u0441\u0442\u043E\u0439\
  \ \u043F\u0440\u0438\u043C\u0435\u0440 \u043D\u0430 \u043E\u0441\u043D\u043E\u0432\
  \u0435 \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0438 `http-conduit`\
  \ \u0432 Haskell. \u0421\u043D\u0430\u0447\u0430\u043B\u0430 \u0443\u0441\u0442\u0430\
  \u043D\u043E\u0432\u0438\u0442\u0435 \u0435\u0451, \u0438\u0441\u043F\u043E\u043B\
  \u044C\u0437\u0443\u044F `cabal install http-\u2026"
lastmod: '2024-03-13T22:44:45.131435-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0440\u0430\u0441\u0441\u043C\
  \u043E\u0442\u0440\u0438\u043C \u043F\u0440\u043E\u0441\u0442\u043E\u0439 \u043F\
  \u0440\u0438\u043C\u0435\u0440 \u043D\u0430 \u043E\u0441\u043D\u043E\u0432\u0435\
  \ \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0438 `http-conduit` \u0432\
  \ Haskell."
title: "\u0417\u0430\u0433\u0440\u0443\u0437\u043A\u0430 \u0432\u0435\u0431-\u0441\
  \u0442\u0440\u0430\u043D\u0438\u0446\u044B"
weight: 42
---

## Как это сделать:
Давайте рассмотрим простой пример на основе библиотеки `http-conduit` в Haskell. Сначала установите её, используя `cabal install http-conduit`. Затем:

```Haskell
import Network.HTTP.Conduit -- Основная сетевая библиотека
import qualified Data.ByteString.Lazy as L -- Нам понадобятся Lazy ByteStrings

-- Функция для загрузки веб-страницы
downloadPage :: String -> IO L.ByteString
downloadPage url = simpleHttp url

main :: IO ()
main = do
    -- Использовать функцию для загрузки страницы
    content <- downloadPage "http://example.com"
    -- Сделать что-то с содержимым, например, распечатать его
    L.putStr content
```

Запустив это, вы увидите HTML `http://example.com` на вашем экране.

## Подробнее
HTTP запросы в Haskell не всегда были такими аккуратными. Старые библиотеки, например `HTTP`, требовали больше шаблонного кода. С `http-conduit`, сложность абстрагирована.

Существуют и другие методы, например команда `wget` в скрипте оболочки или библиотека `requests` в Python. Но они не всегда так же эффективны или выразительны в функциональной среде Haskell.

Под капотом `http-conduit` использует Manager для управления пулом соединений и Keep-Alive для HTTP1.1, делая его более эффективным для множественных запросов.

## См. также
- Для более продвинутого использования `http-conduit`: [http-conduit на Hackage](https://hackage.haskell.org/package/http-conduit)
- Для понимания ByteString: [ByteString на Hackage](https://hackage.haskell.org/package/bytestring)
