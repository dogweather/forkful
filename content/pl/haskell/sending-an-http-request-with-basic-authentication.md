---
title:                "Przesłanie żądania http z podstawowym uwierzytelnieniem"
html_title:           "Haskell: Przesłanie żądania http z podstawowym uwierzytelnieniem"
simple_title:         "Przesłanie żądania http z podstawowym uwierzytelnieniem"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli pracujesz z aplikacjami internetowymi lub tworzysz własną, prawdopodobnie nie unikniesz wysyłania żądań HTTP z uwierzytelnieniem podstawowym. Pozwala to na bezpieczeństwo dostępu do chronionych zasobów, takich jak np. dane użytkowników.

Sending an HTTP request with basic authentication is necessary for accessing protected resources and ensuring security when working with web applications.

## Jak to zrobić

Aby wysłać żądanie HTTP z uwierzytelnieniem podstawowym w języku Haskell, potrzebujesz kilku prostych kroków. Najpierw musisz zaimportować odpowiednie biblioteki:

```Haskell
import Network.HTTP
import Network.HTTP.Headers
import Network.HTTP.Base
import Network.URI
```

Następnie należy utworzyć obiekt `Request` z odpowiednimi parametrami, takimi jak metoda żądania, adres URL i nagłówki:

```Haskell
let url = "https://example.com/api/users/123"
let method = POST
let body = "name=John&age=30"
let headers = [Header HdrAuthorization "Basic <base64encodedCredentials>"]
let request = Request {rqURI = fromJust $ parseURI url, rqMethod = method, rqBody = body, rqHeaders = headers}
```

Warto zauważyć, że nagłówek `HdrAuthorization` musi zawierać poprawnie zakodowane dane uwierzytelniające. Następnie można wysłać żądanie i odbierać odpowiedź:

```Haskell
response <- simpleHTTP request
responseBody <- getResponseBody response
print responseBody
```

W powyższym przykładzie wykorzystujemy funkcje z biblioteki `Network.HTTP`, takie jak `simpleHTTP` i `getResponseBody`, aby wysłać żądanie i odbierać odpowiedź. Można również użyć bardziej zaawansowanych funkcji, takich jak `sendHTTP`, które pozwalają na bardziej szczegółową kontrolę nad żądaniem i odpowiedzią.

## Głębszy przegląd

Wysyłanie żądania HTTP z uwierzytelnieniem podstawowym polega na dołączeniu nagłówka `Authorization` z odpowiednio zakodowanym ciągiem z uwierzytelniającym użytkownika i hasłem. Warto pamiętać, że w celu bezpieczeństwa hasło powinno być zawsze kodowane przed wysłaniem żądania.

Funkcja `encodeCredentials` z biblioteki `Network.HTTP.Headers` służy do kodowania danych uwierzytelniających w standardzie Basic. Należy pamiętać, że należy przekazać ciąg `"Basic "` jako pierwszy argument do tej funkcji, a następnie własny ciąg z uwierzytelniającymi danymi.

Możliwe jest również wysyłanie żądań z innymi metodami uwierzytelniania, takimi jak np. Digest, używając odpowiednich dodatkowych nagłówków i funkcji kodujących.

## Zobacz również

- [Dokumentacja Haskell HTTP](https://hackage.haskell.org/package/HTTP)
- [Wysyłanie żądań HTTP z uwierzytelnianiem w Haskell](https://wiki.haskell.org/HTTP_authenticate)
- [Przykład wysyłania żądania HTTP z uwierzytelnieniem w języku Haskell](https://github.com/mcandre/haskell-httplib/blob/master/http-client.hs)