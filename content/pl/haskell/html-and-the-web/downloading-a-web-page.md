---
date: 2024-01-20 17:44:26.387677-07:00
description: "How to: Czyli po polsku: \"Jak to zrobi\u0107:\" W Haskellu, mo\u017C\
  emy u\u017Cy\u0107 biblioteki `http-conduit` do pobierania stron internetowych.\
  \ Oto przyk\u0142ad u\u017Cycia."
lastmod: '2024-04-05T21:53:36.884164-06:00'
model: gpt-4-1106-preview
summary: ''
title: Pobieranie strony internetowej
weight: 42
---

## How to:
Czyli po polsku: "Jak to zrobić:"

W Haskellu, możemy użyć biblioteki `http-conduit` do pobierania stron internetowych. Oto przykład użycia:

```Haskell
import Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpLBS "http://example.com"
    let statusCode = getResponseStatusCode response
    let body = getResponseBody response
    putStrLn $ "Status code: " ++ show statusCode
    putStrLn $ "Response body: " ++ show body
```

Sample output:

```
Status code: 200
Response body: "<!doctype html>..."
```

## Deep Dive
Czyli po polsku: "Wgłębiamy się":

W przeszłości, by pobrać stronę internetową, można było użyć biblioteki `http`, ale `http-conduit` jest obecnie zalecanym rozwiązaniem, oferującym większą wygodę i funkcje, jak automatyczne przechowywanie sesji czy obsługę połączeń https. Za kulisami, `http-conduit` korzysta z potoków (`conduit`), które efektywnie przetwarzają strumienie danych i mogą obsłużyć zarówno małe, jak i duże odpowiedzi.

## See Also
Czyli po polsku: "Zobacz również":

- Oficjalna dokumentacja `http-conduit`: https://hackage.haskell.org/package/http-conduit
- Opis potoków w Haskellu (`conduit`): https://hackage.haskell.org/package/conduit
- Tutorial Haskell-a: http://learnyouahaskell.com/chapters
