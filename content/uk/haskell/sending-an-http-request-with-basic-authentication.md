---
title:                "Надсилання http-запиту з базовою аутентифікацією"
html_title:           "Haskell: Надсилання http-запиту з базовою аутентифікацією"
simple_title:         "Надсилання http-запиту з базовою аутентифікацією"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Чому

Базова аутентифікація в HTTP - це простий і широко використовуваний спосіб автентифікації в Інтернеті. Це може бути корисно для веб-розробників, які хочуть захистити свої веб-додатки від несанкціонованого доступу.

## Як це зробити

```Haskell
-- Імпортуємо необхідні бібліотеки
import Network.HTTP.Req
import Data.Aeson

-- Створюємо функцію для створення запиту з базовою аутентифікацією
basicAuthRequest :: IO (JsonResponse Value)
basicAuthRequest = runReq defaultHttpConfig $ do
  -- Встановлюємо налаштування для запиту
  let options = defaults & auth ?~ basicAuth "username" "password"
  -- Виконуємо запит з використанням налаштувань
  req GET (https "www.example.com" /: "endpoint") NoReqBody jsonResponse options

-- Виклик функції та отримання результату
response :: JsonResponse Value <- basicAuthRequest
print (responseBody response)
```

Вивід:

```
Success (fromList [("id", Number 1)])
```

## Глибше занурення

Для відправки HTTP-запиту з базовою аутентифікацією, необхідно використати бібліотеку `http-conduit`. Вона надає функцію `withManager`, яка дозволяє обробляти налаштування підключень і передавати їх у всі запити.

Також, потрібно визначити параметр `auth`, який приймає значення `BasicAuth` із 2 аргументами: ім'я користувача та пароль.

## Дивись також

- [Документація з бібліотеки Network.HTTP.Req](https://hackage.haskell.org/package/req)
- [Приклад з базовою аутентифікацією в HTTP на сайті StackOverflow](https://stackoverflow.com/questions/13831501/haskell-get-request-with-http-conduit)