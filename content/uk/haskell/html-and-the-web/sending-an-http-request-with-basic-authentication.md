---
title:                "Надсилання HTTP-запиту з базовою автентифікацією"
aliases:
- /uk/haskell/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:02:18.266685-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту з базовою автентифікацією"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що та Навіщо?
Відправка HTTP-запиту з базовою аутентифікацією — це процес, коли комп'ютер має пройти перевірку перед доступом до ресурсу в інтернеті. Програмісти це роблять для забезпечення безпеки даних, що передаються.

## Як це зробити:
```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Client
import Network.HTTP.Types.Header (hAuthorization)
import Network.HTTP.Base64 (encode)

-- Підготовка з'єднання
prepareRequest url username password = do
  manager <- newManager defaultManagerSettings
  req <- parseRequest url
  let auth = encode $ username <> ":" <> password
      headers = [(hAuthorization, "Basic " <> auth)]
  return $ req { requestHeaders = headers }

-- Відправка запиту
sendRequest :: String -> String -> String -> IO ()
sendRequest url username password = do
  req <- prepareRequest url username password
  manager <- newManager defaultManagerSettings
  response <- httpLbs req manager
  putStrLn $ "Status code: " ++ show (responseStatus response)
  print $ responseBody response

-- Приклад використання
main :: IO ()
main = sendRequest "http://example.com/protected" "user" "pass"
```

Запуск коду надасть результат, подібний наступному:
```
Status code: 200
"{\"example\":\"You accessed protected content!\"}"
```

## Поглиблений Розбір:
Коли інтернет тільки розпочинав своє становлення, безпека часто ігнорувалася. Базова аутентифікація (Base64-кодування логіну і пароля) — одна з перших систем захисту, але зараз вона вважається ненадійною та легкою для зламу. Завжди використовуйте HTTPS для захисту аутентифікаційної інформації. Альтернативами базовій аутентифікації є OAuth, API ключі або ж використання токенів. У даному прикладі, ми використовуємо бібліотеку `http-client` для створення HTTP-запитів та `http-client-tls` для роботи з HTTPS.

## See Also:
- [HTTP Basic Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)
- Офіційна документація `http-client` для Haskell: [http-client on Hackage](https://hackage.haskell.org/package/http-client)
- Документація по безпечній роботі з HTTP у Haskell: [http-client-tls on Hackage](https://hackage.haskell.org/package/http-client-tls)
- Аутентифікація через OAuth у Haskell: [hoauth2 on Hackage](https://hackage.haskell.org/package/hoauth2)
