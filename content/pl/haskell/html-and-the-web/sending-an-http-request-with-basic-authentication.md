---
date: 2024-01-20 18:02:19.506035-07:00
description: "Jak to zrobi\u0107: Oczekiwana odpowied\u017A to kod statusu HTTP wraz\
  \ z tre\u015Bci\u0105 odpowiedzi (je\u015Bli jest dost\u0119pna)."
lastmod: '2024-04-05T21:53:36.884972-06:00'
model: gpt-4-1106-preview
summary: "Oczekiwana odpowied\u017A to kod statusu HTTP wraz z tre\u015Bci\u0105 odpowiedzi\
  \ (je\u015Bli jest dost\u0119pna)."
title: "Wysy\u0142anie zapytania http z podstawow\u0105 autoryzacj\u0105"
weight: 45
---

## Jak to zrobić:
```Haskell
import Network.HTTP.Simple
import Data.ByteString.Base64
import Data.ByteString.Char8 (pack)

-- Zakodowanie loginu i hasła do formatu Base64
createBasicAuthValue :: String -> String -> ByteString
createBasicAuthValue login password =
  "Basic " <> (encode . pack $ login ++ ":" ++ password)

-- Przygotowanie żądania z uwierzytelnieniem
prepareRequest :: String -> String -> String -> Request
prepareRequest url login password =
  setRequestMethod "GET" $
  setRequestURL (pack url) $
  addRequestHeader "Authorization" (createBasicAuthValue login password) $
  defaultRequest

-- Wysłanie żądania HTTP
sendHttpRequest :: Request -> IO ()
sendHttpRequest request = do
  response <- httpBS request
  putStrLn $ "Stan odpowiedzi: " ++ show (getResponseStatusCode response)
  print $ getResponseBody response

-- Przykład użycia
main :: IO ()
main = do
  let url = "http://example.com/protected"
  let login = "user"
  let password = "pass"
  let request = prepareRequest url login password
  sendHttpRequest request
```

Oczekiwana odpowiedź to kod statusu HTTP wraz z treścią odpowiedzi (jeśli jest dostępna):

```
Stan odpowiedzi: 200
"Odpowiedź z zabezpieczonego zasobu"
```

## Głębiej:
Bazowe uwierzytelnienie HTTP (Basic Authentication) jest prostą metodą bezpieczeństwa używaną od czasów HTTP/1.0. Dziś, choć uznaje się je za mniej bezpieczne niż bardziej zaawansowane techniki jak OAuth, nadal jest powszechnie stosowane, głównie z powodu prostoty implementacji. W Haskell możemy korzystać z gotowych bibliotek jak `http-conduit` czy `http-simple` do obsługi żądań HTTP, włączając w to uwierzytelnienie. W powyższym przykładzie użyto `http-simple` do tworzenia i wysyłania żądania z nagłówkiem uwierzytelniającym `Authorization`. Kodowanie `login:password` do formatu Base64 jest konieczne, gdyż taki format jest oczekiwany przez standard Basic Authentication.

Alternatywy dla Basic Authentication to m.in. Digest Authentication, TLS Client Authentication oraz wspomniany OAuth, które oferują różne poziomy złożoności i bezpieczeństwa.

## Zobacz również:
- [Haskell Network.HTTP.Simple Documentation](https://hackage.haskell.org/package/http-conduit-2.3.8/docs/Network-HTTP-Simple.html)
- [RFC 7617 - The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)
- [Haskell http-conduit Package](https://hackage.haskell.org/package/http-conduit)
