---
title:                "Загрузка веб-страницы"
aliases:
- /ru/haskell/downloading-a-web-page.md
date:                  2024-01-28T23:57:29.220170-07:00
model:                 gpt-4-0125-preview
simple_title:         "Загрузка веб-страницы"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/haskell/downloading-a-web-page.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?

Загрузка веб-страницы означает получение её данных через интернет; это как сохранение копии для чтения или обработки локально. Программисты делают это для сбора содержимого, взаимодействия с веб-сервисами или создания зеркал сайтов.

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
