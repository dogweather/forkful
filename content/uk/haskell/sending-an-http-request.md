---
title:                "Надсилання http запиту"
html_title:           "Haskell: Надсилання http запиту"
simple_title:         "Надсилання http запиту"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Чому

Користувачі можуть відправляти HTTP-запити, щоб звернутися до серверів у мережі Інтернет і отримати необхідну інформацію або взаємодіяти з веб-сервісами та API. Навіть якщо ви не розробник, ви, наприклад, можете використовувати HTTP-запити, щоб скачати музику або відео з Інтернету.

## Як

Для того, щоб відправити HTTP-запит у Haskell, вам потрібно встановити бібліотеку `http-conduit`, яка дозволить нам взаємодіяти з серверами через протокол HTTP. За допомогою функції `simpleHttp` ми можемо відправити GET-запит до URL і отримати відповідь у вигляді `ByteString`.

```Haskell
import Network.HTTP.Conduit

main = do
  response <- simpleHttp "http://hackage.haskell.org/package/http-conduit"
  print response

-- Вивід: 
-- "200 OK" 
-- "Foo Bar Baz"
```

## Глибші дослідження

За допомогою функції `simpleHttp` ми можемо відправляти лише GET-запити, але є можливість відправляти інші типи запитів, такі як POST, PUT, DELETE тощо, за допомогою функцій `httpLbs` або `httpLbsRequest`. Крім того, HTTP-запити можуть містити різноманітні заголовки та параметри, що дозволяє більш гнучку взаємодію з серверами.

## Дивись також

- [Офіційна документація по `http-conduit`](https://www.stackage.org/haddock/lts-7.24/http-conduit/Network-HTTP-Conduit.html)
- [Приклади відправки HTTP-запитів за допомогою `http-conduit`](https://github.com/snoyberg/http-client/blob/master/http-client-tests/Main.hs)