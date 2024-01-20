---
title:                "Надсилання http-запиту"
html_title:           "Arduino: Надсилання http-запиту"
simple_title:         "Надсилання http-запиту"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Що це таке і навіщо це потрібно?

Відправлення HTTP-запиту -  це процес передачі повідомлень від клієнта до сервера в Інтернеті або будь-якій іншій мережі, використовуючи протокол HTTP. Програмісти роблять це для взаємодії з веб-сервісами: отримання даних, надсилання даних тощо.

## Як це зробити?

Нижче наведено код для відправлення простого HTTP GET запиту в Haskell:

```Haskell 
import Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpLBS "http://example.com"
    print $ getResponseStatusCode response
    print $ getResponseStatusMessage response
    print $ getResponseBody response
```

У відповідь отримаєте статус-код відповіді, повідомлення про статус та тіло відповіді. 

## Більш глибоко 

1. В історичному контексті, HTTP був створений для передачі гіпертекстових документів. Зараз, він використовується для передачі різних типів даних.
2. Є різні альтернативи HTTP, такі як WebSocket, gRPC, GraphQL тощо. Вибір залежить від специфіки завдання.
3. Процес відправлення HTTP-запиту в Haskell полягає в надсиланні запиту через TCP/IP з використанням бібліотеки, яка абстрагує низькорівневі деталі.

## Див. також 

1. Офіційні документи Haskell: https://www.haskell.org/
2. Огляд HTTP в Haskell : https://hackage.haskell.org/package/http