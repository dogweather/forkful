---
title:                "Haskell: Wysyłanie żądania http z podstawową autoryzacją"
simple_title:         "Wysyłanie żądania http z podstawową autoryzacją"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Dlaczego

Wysyłanie żądań HTTP z podstawową autoryzacją może być bardzo przydatne w wielu przypadkach. Może to być potrzebne, gdy chcemy uzyskać dostęp do chronionych zasobów lub gdy korzystamy z API, które wymaga uwierzytelnienia.

## Jak to zrobić

Możemy użyć biblioteki "http-conduit" w Haskellu, aby wysłać żądanie HTTP z podstawową autoryzacją. Najpierw musimy zaimportować bibliotekę:

```Haskell
import Network.HTTP.Simple
```

Następnie musimy utworzyć zapytanie HTTP i dodać do niego nagłówek autoryzacji:

```Haskell
import qualified Data.ByteString.Char8 as BS
request <- parseRequest "http://example.com/api"
let username = "user123"
let password = "secret"
let authHeaderValue = BS.pack (username ++ ":" ++ password)
let basicAuthHeader = ("Authorization", BS.pack ("Basic " ++ (BS.unpack (Data.ByteString.Base64.encode authHeaderValue))))
let requestWithAuthHeader = setRequestHeader basicAuthHeader request
```

Teraz możemy wysłać żądanie i otrzymać odpowiedź:

```Haskell
response <- httpJSON requestWithAuthHeader
```

Możemy także obsłużyć błędy, które mogą wystąpić przy wysyłaniu żądania:

```Haskell
case getResponseStatusCode response of
    200 -> print "Success"
    _ -> print "Error"
```

Przykładowy output: "Success".

## Deep Dive

Podczas wysyłania żądania HTTP z podstawową autoryzacją, najważniejsze jest wygenerowanie poprawnego nagłówka autoryzacyjnego. Nagłówek ten musi być w formacie "Basic username:password" i powinien być zakodowany w formacie Base64.

W przykładzie powyżej, używamy funkcji "Data.ByteString.Base64.encode" z biblioteki "base64-bytestring" do zakodowania nagłówka autoryzacyjnego.

## Zobacz również

- [Dokumentacja biblioteki "http-conduit"](https://hackage.haskell.org/package/http-conduit)
- [Przykładowy kod wykorzystujący podstawową autoryzację w bibliotece "http-conduit"](https://github.com/snoyberg/conduit/blob/master/http-conduit/examples/tls.hs)
- [Inny sposób na autoryzację w bibliotece "http-conduit"](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/Simple%20HTTP%20Client%20Request)