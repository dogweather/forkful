---
title:                "Надсилання http-запиту з основною автентифікацією"
html_title:           "Haskell: Надсилання http-запиту з основною автентифікацією"
simple_title:         "Надсилання http-запиту з основною автентифікацією"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Що & Чому?

Відправлення HTTP-запиту із базовою аутентифікацією - це коли програміст надсилає запит до веб-сервера, використовуючи базову аутентифікацію для перевірки своїх даних доступу. Це часто робиться для роботи з захищеними ресурсами або для виконання авторизованих дій на веб-сайтах.

# Як писати код?

Відправлення HTTP-запиту із базовою аутентифікацією може бути легко реалізовано у Haskell за допомогою бібліотеки Network.HTTP.Conduit. Перш за все, потрібно імпортувати потрібні модулі.

```Haskell
import Network.HTTP.Conduit
import Network.HTTP.Types.Header (hAuthorization)
import qualified Data.ByteString.Char8 as BS
```

Далі можна створити базову аутентифікаційну стрічку, де першим параметром є ім'я користувача, а другим - пароль.

```Haskell
let authString = BS.pack "username:password"
```

Тепер можна створити запит і додати до нього заголовок з базовою аутентифікацією.

```Haskell
initRequest <- parseRequest "http://www.example.com"
let request = initRequest
              { requestHeaders =
                  [(hAuthorization, "Basic " <> (BS.pack $ base64Encode authString))]
              }
```

Нарешті, можна виконати запит і отримати відповідь від сервера.

```Haskell
response <- httpLbs request manager
putStrLn $ show $ responseBody response
```

Вищенаведений код відправляє GET-запит до веб-сайту із базовою аутентифікацією і виводить отриману відповідь в консоль.

# Поглиблене вивчення

- Історичний контекст: базова аутентифікація була першою формою аутентифікації для HTTP-запитів і зберігає свою актуальність до сьогоднішнього дня.

- Альтернативні підходи: окрім базової аутентифікації, існують інші методи аутентифікації, наприклад, OAuth.

- Деталі реалізації: бібліотека Network.HTTP.Conduit дозволяє використовувати не тільки базову аутентифікацію, але і інші методи, такі як Digest або NTLM.

# Дивись також

- [Network.HTTP.Conduit документація](https://hackage.haskell.org/package/http-conduit/docs/Network-HTTP-Conduit.html)
- [HTTP-аутентифікація на Вікіпедії](https://uk.wikipedia.org/wiki/HTTP-%D0%B0%D1%83%D1%82%D0%B5%D0%BD%D1%82%D0%B8%D1%84%D1%96%D0%BA%D0%B0%D1%86%D1%96%D1%8F)