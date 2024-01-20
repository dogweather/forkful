---
title:                "Wysyłanie żądania http"
html_title:           "Arduino: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wysyłanie żądania HTTP to podstawowy sposób, w jaki aplikacje infrastuktury internetowej. To zasadniczo korespondencja, gdzie aplikacja prosi o określone zasoby od serwera. Programiści robią to, aby uzyskać dane potrzebne dla ich aplikacji - to jak zadawanie pytania i otrzymywanie odpowiedzi.

## Jak to zrobić:

Przykładowy kod, który pokazuje, jak wysłać prosty GET żądanie w Haskellu może wyglądać następująco:

```Haskell
import Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpLBS "http://httpbin.org/get"

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    L8.putStrLn $ getResponseBody response
```

W wyniku powyższego kodu na wyjściu otrzymamy kod statusu, typ zawartości oraz treść odpowiedzi.

## Szybki kurs:

Sterowanie HTTP wyewoluowało na przestrzeni lat od prostych zapytań GET do obecnych skomplikowanych komunikacji API. Haskell, jako potężne narzędzie do programowania funkcjonalnego, nie pozostaje w tyle, oferując różne pakiety, takie jak `http-simple` i `http-conduit`, które umożliwiają prostą i tę proces.

Alternatywą dla wysyłania zapytań HTTP w Haskellu mogą być inne języki programowania, takie jak Python lub JavaScript, które oferują podobne funkcje. Wybór zależy od kontekstu projektu i preferencji programisty.

Szczegółowo, wysyłanie żądania HTTP w Haskellu opiera się na konstrukcji monad IO, która jest gęsto używana do opisywania operacji wejścia/wyjścia, takich jak te związane z siecią.

## Zobacz też:

Jeśli chcesz dowiedzieć się więcej o tej tematyce, polecam następujące źródła:

- Dokumentacja pakietu `http-conduit`: https://hackage.haskell.org/package/http-conduit
- Dokumentacja pakietu `http-simple`: https://hackage.haskell.org/package/http-simple
- "Real World Haskell" autorstwa Briana O'Sullivana, Dona Stewarta i Johna Goera: https://book.realworldhaskell.org/