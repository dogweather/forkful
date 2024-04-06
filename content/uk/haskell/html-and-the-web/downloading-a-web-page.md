---
date: 2024-01-20 17:44:09.640288-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0412\u0438\u0432\u0456\u0434 \u043C\u043E\u0436\u0435 \u0432\u0438\u0433\u043B\
  \u044F\u0434\u0430\u0442\u0438 \u0442\u0430\u043A."
lastmod: '2024-04-05T21:53:49.535025-06:00'
model: gpt-4-1106-preview
summary: "\u0412\u0438\u0432\u0456\u0434 \u043C\u043E\u0436\u0435 \u0432\u0438\u0433\
  \u043B\u044F\u0434\u0430\u0442\u0438 \u0442\u0430\u043A."
title: "\u0417\u0430\u0432\u0430\u043D\u0442\u0430\u0436\u0435\u043D\u043D\u044F \u0432\
  \u0435\u0431-\u0441\u0442\u043E\u0440\u0456\u043D\u043A\u0438"
weight: 42
---

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
