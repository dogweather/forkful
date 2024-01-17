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

## Czym jest wysyłanie żądania HTTP i dlaczego programiści to robią?
Wysyłanie żądania HTTP (Hypertext Transfer Protocol) jest procesem przesyłania danych między serwerem a klientem. Programiści mogą wysyłać żądania HTTP w celu pobrania danych z serwera, np. plików lub informacji z bazy danych.

## Jak to zrobić:
```Haskell
-- Przykładowe wysłanie żądania GET z biblioteką HTTP w języku Haskell
import Network.HTTP

main = do
  response <- simpleHTTP (getRequest "https://www.example.com")
  putStrLn $ "Kod odpowiedzi: " ++ (show . rspCode $ response)
  putStr $ "Zawartość odpowiedzi: " ++ (rspBody $ response)
```
W odpowiedzi otrzymujemy kod odpowiedzi oraz zawartość odpowiedzi w postaci pliku lub danych tekstowych.

## Głębszy Wgląd:
Wysyłanie żądania HTTP jest kluczowym elementem komunikacji sieciowej, umożliwiającym pobieranie danych z serwera. Inne popularne metody do wysyłania żądań HTTP to np. curl lub biblioteka "httplib" w Pythonie. Implementacja funkcji wysyłającej żądanie HTTP w języku Haskell jest możliwa dzięki bibliotece Network.HTTP, która dostarcza różne narzędzia do budowania i wysyłania żądań.

## Zobacz również:
- Opis funkcji Network.HTTP w dokumentacji języka Haskell: https://hackage.haskell.org/package/HTTP
- Poradnik "Jak przygotować żądanie HTTP w języku Haskell": https://www.tweag.io/blog/2015-09-10-requests/