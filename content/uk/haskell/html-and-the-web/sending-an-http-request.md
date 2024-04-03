---
date: 2024-01-20 17:59:58.552904-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0423 Haskell \u0434\u043B\u044F HTTP-\u0437\u0430\u043F\u0438\u0442\u0456\u0432\
  \ \u043C\u043E\u0436\u043D\u0430 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\
  \u043E\u0432\u0443\u0432\u0430\u0442\u0438 \u043F\u0430\u043A\u0435\u0442 `http-conduit`.\
  \ \u041E\u0441\u044C \u043F\u0440\u0438\u043A\u043B\u0430\u0434 \u043F\u0440\u043E\
  \u0441\u0442\u043E\u0433\u043E GET-\u0437\u0430\u043F\u0438\u0442\u0443."
lastmod: '2024-03-13T22:44:49.352848-06:00'
model: gpt-4-1106-preview
summary: "\u0423 Haskell \u0434\u043B\u044F HTTP-\u0437\u0430\u043F\u0438\u0442\u0456\
  \u0432 \u043C\u043E\u0436\u043D\u0430 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\
  \u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438 \u043F\u0430\u043A\u0435\u0442\
  \ `http-conduit`."
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443"
weight: 44
---

## Як це зробити:
У Haskell для HTTP-запитів можна використовувати пакет `http-conduit`. Ось приклад простого GET-запиту:

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpBS "http://httpbin.org/get"
    print (getResponseBody response)
```

Коли ви запустите цей код, вивід буде приблизно таким:

```
"{\n  \"args\": {}, \n  \"headers\": {\n    \"Accept-Encoding\": \"gzip\", \n    \"Host\": \"httpbin.org\", \n    \"User-Agent\": \"haskell http-conduit\"\n  }, \n  \"origin\": \"[your IP]\", \n  \"url\": \"https://httpbin.org/get\"\n}\n"
```

## Поглиблено:
Використовуючи `http-conduit`, ви насолоджуєтеся автоматичним управлінням пулом з'єднань і можливістю відправляти асинхронні запити. Цей пакет побудований на `conduit`, що забезпечує потокову обробку даних і ефективне управління ресурсами.

Як альтернативу можна розглянути такі пакети, як `wreq` або нижньорівневу бібліотеку `http-client`. У минулому для HTTP-взаємодій в Haskell часто використовувалася бібліотека `network`.

Ключове в уточненні HTTP-запиту в Haskell — це побудова правильного Request об'єкту, управління заголовками, кукі та методами запитів. Модуль `Network.HTTP.Simple` дозволяє з легкістю керувати цими аспектами.

## Дивіться також:
- [http-conduit на Hackage](https://hackage.haskell.org/package/http-conduit)
- [Wreq: a Haskell Web Client Library](https://hackage.haskell.org/package/wreq)
