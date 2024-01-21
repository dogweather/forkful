---
title:                "Завантаження веб-сторінки"
date:                  2024-01-20T17:44:09.640288-07:00
model:                 gpt-4-1106-preview
simple_title:         "Завантаження веб-сторінки"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Що та Чому?
Завантаження веб-сторінки - це процес отримання її вмісту через HTTP. Програмісти роблять це для аналізу контенту, моніторингу змін чи збору даних.

## Як це зробити:
```Haskell
import Network.HTTP.Conduit (simpleHttp)

main :: IO ()
main = do
    webpage <- simpleHttp "http://example.com"
    putStrLn $ take 1000 $ show webpage  -- Показуємо перші 1000 символів вмісту
```

Вивід може виглядати так:
```
"<html>...
... зміст веб-сторінки ...
...</html>"
```

## Поглиблений розгляд:
У мінулому, завантаження веб-сторінки в Haskell часто виконувалось за допомогою пакету `HTTP`. З часом, `http-conduit` став популярний через своє зручне управління з'єднаннями та підтримку SSL. Окрім `http-conduit`, існують інші бібліотеки, як-от `wreq` чи `req`, кожна зі своїми особливостями. Обравши `Network.HTTP.Conduit`, ви отримуєте могутніші інструменти для роботи з HTTP в Haskell, що особливо корисно при складніших завданнях, таких як обробка редиректів чи використання проксі.

## Додаткові ресурси:
- [The http-conduit package on Hackage](https://hackage.haskell.org/package/http-conduit)
- [The Haskell Network.HTTP documentation](https://hackage.haskell.org/package/HTTP)