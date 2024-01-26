---
title:                "Wysyłanie zapytania http z podstawową autoryzacją"
date:                  2024-01-20T18:02:19.506035-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wysyłanie zapytania http z podstawową autoryzacją"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Wysyłanie zapytania HTTP z podstawowym uwierzytelnianiem to proces przekazywania loginu i hasła by uzyskać dostęp do zabezpieczonych zasobów. Programiści robią to, aby bezpiecznie komunikować się z serwerami i API, które wymagają autoryzacji.

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
