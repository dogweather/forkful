---
title:                "Przesyłanie żądania http z podstawową uwierzytelnieniem"
html_title:           "Haskell: Przesyłanie żądania http z podstawową uwierzytelnieniem"
simple_title:         "Przesyłanie żądania http z podstawową uwierzytelnieniem"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Wysyłanie żądania HTTP z podstawową autoryzacją to proces, w którym programista wysyła dane do serwera z uwierzytelnieniem poprzez podanie loginu i hasła. Programiści wykorzystują to w celu bezpiecznego i autoryzowanego dostępu do zasobów serwera, takich jak bazy danych czy pliki.

## Jak to zrobić?

Przykładowy kod w Haskell wykorzystujący funkcję `httpLBS` z biblioteki `http-conduit` do wysłania żądania HTTP z podstawową autoryzacją:
```Haskell
{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Conduit
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
    request <- parseRequest "https://www.example.com"
    let requestWithAuth = applyBasicAuth "myUsername" "myPassword" request
    response <- httpLBS requestWithAuth
    liftIO $ print response
```
Przykładowy output (przy założeniu, że użytkownik i hasło są poprawne):
```
Response {
    responseStatus = Status {statusCode = 200, statusMessage = "OK"},
    responseVersion = HTTP/1.1,
    responseHeaders = [...],
    responseBody = (raw data),
    responseCookieJar = (empty cookie jar)
    }
```

## Głębszy Zanurzenie

Funkcja `applyBasicAuth` została wprowadzona w Haskellu w wersji 7.6 w celu ułatwienia wysyłania żądań HTTP z podstawową autoryzacją. Alternatywą dla tej funkcji jest bezpośrednie wykorzystanie nagłówka `Authorization` w żądaniu HTTP. Implementacja autoryzacji podstawowej polega na kodowaniu loginu i hasła przy użyciu kodowania Base64. Wysyłane dane są wciąż podatne na ataki typu "man-in-the-middle", dlatego zaleca się wykorzystanie bardziej zaawansowanych metod uwierzytelnienia, takich jak OAuth.

## Zobacz także

- [Dokumentacja biblioteki http-conduit dla funkcji `applyBasicAuth`](https://hackage.haskell.org/package/http-conduit-2.3.8.2/docs/Network-HTTP-Conduit.html#v:applyBasicAuth)
- [Przykładowe kody w innych językach programowania dla autoryzacji podstawowej](https://www.httpwatch.com/httpgallery/authentication/#http_basicauth)