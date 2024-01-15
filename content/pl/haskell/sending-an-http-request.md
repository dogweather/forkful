---
title:                "Wysyłanie żądania http"
html_title:           "Haskell: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli pracujesz z siecią lub tworzysz aplikację internetową, możliwe, że w pewnym momencie będziesz musiał wysłać zapytanie HTTP. Napisanie kodu w języku Haskell pozwoli Ci na wykorzystanie silnych typów i funkcjonalnego podejścia, co ułatwi i uprzyjemni pracę z zapytaniami HTTP.

## Jak to zrobić

Aby wysłać zapytanie HTTP w Haskell, potrzebujesz użyć narzędzia o nazwie `http-client`. Przykład kodu poniżej pokazuje, jak stworzyć GET request do strony Google i wypisać otrzymaną odpowiedź:

```Haskell
import Network.HTTP.Client

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings
  request <- parseUrlThrow "http://www.google.com"
  response <- httpLbs request manager
  putStrLn $ "Odpowiedź: " ++ show response
```
Output:
```
Odpowiedź: Response {responseStatus = Status {statusCode = 200, statusMessage = "OK"}, responseVersion =HTTP/1.1, responseHeaders = [("Content-Type","text/html; charset=ISO-8859-1"),("Date","Tue, 16 Feb 2021 00:00:00 GMT"),("Server","Google Frontend"),("Content-Length","12345")], responseBody = " ..." }
```

Możesz także dodać dodatkowe informacje do swojego requestu, np. nagłówki czy parametry. Szczegółowe informacje na ten temat znajdziesz w dokumentacji `http-client`.

## Dogłębnie

Za pomocą narzędzia `http-client`, Haskell oferuje nam wiele możliwości w obsłudze zapytań HTTP. Możemy wysyłać różnego rodzaju requesty, w tym także POST lub PUT, dodawać autoryzację czy konfigurować proxy. Dodatkowo, `http-client` jest wyjątkowo wydajny i współpracuje z innymi bibliotekami, np. `conduit` czy `cookie`.

Jednak warto pamiętać, że narzędzie to obsługuje wyłącznie synchroniczne zapytania. Jeśli potrzebujesz wykorzystać asynchroniczne requesty, powinieneś spojrzeć w stronę biblioteki `http-conduit`.

## Zobacz również

- Dokumentacja `http-client`: https://hackage.haskell.org/package/http-client
- Przykładowe kody z nastawieniami do requestów: https://github.com/snoyberg/http-client/tree/master/http-client-tls
- Dokumentacja `http-conduit`: https://www.stackage.org/package/http-conduit/docs/Network-HTTP-Conduit.html