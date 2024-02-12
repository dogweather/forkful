---
title:                "Надсилання HTTP-запиту"
aliases:
- /uk/haskell/sending-an-http-request.md
date:                  2024-01-20T17:59:58.552904-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Що & Навіщо?
HTTP-запит використовується для взаємодії з веб-сервісами: отримання та надсилання даних. Програмісти роблять це, щоб їхні програми могли спілкуватися з іншими системами через Інтернет.

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
