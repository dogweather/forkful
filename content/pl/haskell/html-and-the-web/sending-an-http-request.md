---
title:                "Wysyłanie żądania HTTP"
aliases:
- /pl/haskell/sending-an-http-request/
date:                  2024-01-20T17:59:51.773352-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wysyłanie żądania HTTP"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wysyłanie żądania HTTP to sposób, w jaki Twój program może komunikować się z serwerem w internecie, np. aby pobrać dane lub wysłać informacje. Programiści robią to, by integrować zewnętrzne usługi i źródła danych, które dodają wartość do ich aplikacji.

## Jak to zrobić:

W Haskellu do wysyłania żądań HTTP możemy użyć biblioteki `http-conduit`. Poniżej znajdziesz przykład prostego żądania GET.

```haskell
import Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpBS "http://httpbin.org/get"
    putStrLn $ "The status code was: " ++ show (getResponseStatusCode response)
    putStrLn "The headers were:"
    print $ getResponseHeaders response
    putStrLn "The body was:"
    BS.putStrLn $ getResponseBody response
```

Przy uruchomieniu, wynik będzie podobny do poniższego:

```
The status code was: 200
The headers were:
[(CI.mk "Content-Type","application/json"), ...]
The body was:
{
  ...
}
```

## Deep Dive

W roku 2011, HTTP Conduit został stworzony jako część większej biblioteki `conduit` dla języka Haskell, ułatwiająca obsługę strumieni danych. Alternatywami dla `http-conduit` mogą być `http-client` lub bardziej niskopoziomowa `http` - ale `conduit` oferuje dużą elastyczność, zwłaszcza przy obsłudze dużych danych.

Podczas wysyłania zapytania, `http-conduit` tworzy połączenie z podanym URL, wysyła żądanie i czeka na odpowiedź. Jest to asynchroniczna operacja, która w Haskellu jest obsługiwana za pomocą monad IO, pozwalając na efekty uboczne w czysto funkcyjnym środowisku.

Alternatywne biblioteki jak `Wreq` lub `Req` też pozwalają na wysyłanie żądań HTTP, często z dodatkowymi udogodnieniami typu automatyczne przetwarzanie JSON, ale mogą być mniej elastyczne przy pracy ze specyficznymi rodzajami powiązań HTTP.

## See Also

- Biblioteka `http-conduit`: https://www.stackage.org/package/http-conduit
- Dokumentacja `http-client`, której `http-conduit` używa: https://www.stackage.org/package/http-client
- `conduit` tutorial, pokazujący jak zarządzać strumieniami danych: https://www.yesodweb.com/book/conduit
- Porównanie bibliotek HTTP dla Haskell: https://wiki.haskell.org/Network_programming#Libraries_for_HTTP_client_programming
