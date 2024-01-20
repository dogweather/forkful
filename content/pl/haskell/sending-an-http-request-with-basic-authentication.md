---
title:                "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
html_title:           "Arduino: Wysyłanie żądania http z podstawowym uwierzytelnieniem"
simple_title:         "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wysyłanie żądania HTTP z podstawowym uwierzytelnieniem polega na umieszczaniu loginu i hasła w nagłówku HTTP. Programiści robią to, aby zabezpieczyć dostęp do swoich aplikacji.

## Jak to zrobić:
Poniżej znajduje się przykład, jak można to zrobić w języku Haskell. Używamy w tym celu biblioteki `http-conduit`.

```Haskell
import Network.HTTP.Simple
import Network.HTTP.Client as Client (applyBasicAuth)

main :: IO ()
main = do
    let request' = setRequestMethod "GET" 
                 $ setRequestPath "/api/data" 
                 $ setRequestHost "www.example.com"
                 $ setRequestSecure True
                 $ setRequestPort 443 
                 $ defaultRequest
    let request = Client.applyBasicAuth "username" "password" request'

    response <- httpLBS request
    putStrLn $ "Status: " ++ show (getResponseStatusCode response)
    print $ getResponseBody response
```
Uruchomienie powyższego programu wyśle żądanie HTTP na stronę `www.example.com/api/data`, używając uwierzytelnienia Basic Auth.

## Dogłębna analiza
Pierwotnie protokół uwierzytelnienia Basic Auth został zaprojektowany do użytku w sieciach wewnętrznych, gdzie bezpieczeństwo nie było wielkim problemem. Z biegiem czasu, w miarę jak Internet stawał się coraz bardziej popularny, protokół ten zaczął być używany w szerszym kontekście, mimo że nie był to pierwotny zamiar.

Alternatywą dla uwierzytelnienia Basic jest uwierzytelnienie `Digest`, które jest nieco bezpieczniejsze, ponieważ hasło jest hashowane zamiast być przesyłane w czystej postaci. Niemniej jednak, oba te metody są uważane za mniej bezpieczne niż nowoczesne metody uwierzytelnienia, takie jak `OAuth`.

W przypadku implementacji w Haskell, używamy `applyBasicAuth` z pakietu http-client, który dodaje nagłówek `Authorization` do żądania HTTP. Ten nagłówek zawiera nazwę użytkownika i hasło, zakodowane w formacie `Base64`.

## Zobacz także:
1. [http-conduit](https://hackage.haskell.org/package/http-conduit)
2. [http-client](https://hackage.haskell.org/package/http-client)
3. [Auth basic RFC](https://tools.ietf.org/html/rfc7617)
4. [OAuth](https://oauth.net/)

Przeczytaj powyższe linki, aby dowiedzieć się więcej na temat programowania HTTP w Haskellu, uwierzytelniania Basic Auth i jego alternatyw.