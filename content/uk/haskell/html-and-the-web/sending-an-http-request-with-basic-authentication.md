---
date: 2024-01-20 18:02:18.266685-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : ."
lastmod: '2024-03-13T22:44:49.358016-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443 \u0437 \u0431\u0430\u0437\u043E\u0432\u043E\u044E \u0430\
  \u0432\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\u0446\u0456\u0454\u044E"
weight: 45
---

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
